-module(eVariable).
-export([fetchVariables/1]).

-include("eRecordDefinition.hrl").

-spec fetchVariables(eAST()) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables(AST) ->
    {AST3, VarTypes, InitCode} = fetchVariables(prepareStructInitExpression(AST), [], {#{}, [], true}),
    {AST3, VarTypes, InitCode}.

-spec prepareStructInitExpression(eAST()) -> eAST().
prepareStructInitExpression([#functionRaw{statements = Expressions} = F | Rest]) ->
    [F#functionRaw{statements = fixStructInitAST(Expressions)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([#structRaw{fields = Expressions} = S | Rest]) ->
    [S#structRaw{fields = fixStructInitAST(Expressions)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([#variableDefinition{initialValue = InitialValue} = V | Rest]) ->
    [V#variableDefinition{initialValue = fixStructInit(InitialValue)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([]) ->
    [].

-spec fixStructInitAST(eAST()) -> eAST().
fixStructInitAST(Lst) ->
    eUtil:expressionMap(fun fixStructInit/1, Lst).

-spec fixStructInit(eExpression()) -> eExpression().
fixStructInit(#structInitializeExpressionRaw{name = Name, fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = structInitToMap(Fields),
    #structInitializeExpression{name = Name, fieldNames = FieldNames, fieldValueMap = InitExprMap, line = Line};
fixStructInit(#arrayInitializeExpression{elements = Elements} = A) ->
    A#arrayInitializeExpression{elements = fixStructInitAST(Elements)};
fixStructInit(#variableDefinition{initialValue = InitialValue} = V) ->
    V#variableDefinition{initialValue = fixStructInit(InitialValue)};
fixStructInit(#operatorExpression2{operand1 = Operand1, operand2 = Operand2} = O) ->
    O#operatorExpression2{operand1 = fixStructInit(Operand1), operand2 = fixStructInit(Operand2)};
fixStructInit(#operatorExpression1{operand = Operand} = O) ->
    O#operatorExpression1{operand = fixStructInit(Operand)};
fixStructInit(Any) ->
    Any.

-spec structInitToMap([eExpression()]) -> {[#variableReference{}], #{atom() := eExpression()}}.
structInitToMap(Expressions) ->
    structInitToMap(Expressions, [], #{}).

-spec structInitToMap([#operatorExpression2{}], [#variableReference{}], #{atom() := eExpression()}) -> {[#variableReference{}], #{atom() := eExpression()}}.
structInitToMap([#operatorExpression2{operator = assign, operand1 = #variableReference{name = Field} = Operand1, operand2 = Val} | Rest], FieldNames, ExprMap) ->
    structInitToMap(Rest, [Operand1 | FieldNames], ExprMap#{Field => fixStructInit(Val)});
structInitToMap([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% In function expressions, the init code of defvar can not be simply fetched out from the code,
%% it should be replaced as assignment in the same place.
-spec fetchVariables(eAST(), eAST(), {variableTypeMap(), eAST(), boolean()}) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables([#variableDefinition{name = Name, type = Type, line = Line, initialValue = InitialValue} | Rest], NewAST, {VarTypes, InitCode, CollectInitCode}) ->
    ensureNoNameConflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            fetchVariables(Rest, NewAST, {VarTypes#{Name => Type}, appendToAST(InitCode, Name, InitialValue, Line), CollectInitCode});
        false ->
            fetchVariables(Rest, appendToAST(NewAST, Name, InitialValue, Line), {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
fetchVariables([#functionRaw{name = Name, returnType = Ret, parameters = Params, statements = Expressions, line = Line} | Rest], NewAST, {GlobalVars, _, _} = Context) ->
    {[], ParamVars, ParamInitCode} = fetchVariables(Params, [], {#{}, [], true}),
    eUtil:assert(ParamInitCode =:= [], {Line, "function parameters can not have default value"}),
    {NewExpressions, FunVarTypes, []} = fetchVariables(Expressions, [], {ParamVars, [], false}),
    %% local variables should have different names from global variables
    checkVariableConflict(GlobalVars, FunVarTypes),
    %% label names should be different from variables, because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label end, Expressions),
    checkLabelConflict(Labels, GlobalVars, FunVarTypes),
    FunctionType = #functionType{parameters = getValuesByDefinitions(Params, ParamVars), ret = Ret, line = Line},
    Function = #function{name = Name, variableTypeMap = FunVarTypes, statements = NewExpressions, parameterNames = variableDefinitionToReference(Params), line = Line, type = FunctionType},
    fetchVariables(Rest, [Function | NewAST], Context);
fetchVariables([#structRaw{name = Name, fields = Fields, line = Line} | Rest], NewAST, Context) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetchVariables(Fields, [], {#{}, [], true}),
    {_, FieldInitMap} = structInitToMap(StructInitCode),
    S = #struct{name = Name, fieldTypeMap = FieldTypes, fieldNames = variableDefinitionToReference(Fields), fieldDefaultValueMap = FieldInitMap, line = Line},
    fetchVariables(Rest, [S | NewAST], Context);
fetchVariables([Any | Rest], NewAST, Context) ->
    fetchVariables(Rest, [Any | NewAST], Context);
fetchVariables([], NewAST, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAST), VarTypes, lists:reverse(InitCode)}.

-spec appendToAST(eAST(), atom(), eExpression(), integer()) -> eAST().
appendToAST(AST, VariableName, InitialValue, Line) when InitialValue =/= none ->
    [#operatorExpression2{operator = assign, operand1 = #variableReference{name = VariableName, line = Line}, operand2 = InitialValue, line = Line} | AST];
appendToAST(AST, _, _, _) ->
    AST.

-spec checkLabelConflict([eExpression()], variableTypeMap(), variableTypeMap()) -> ok.
checkLabelConflict([#gotoLabel{name = Name, line = Line} | Rest], GlobalVars, LocalVars) ->
    ensureNoNameConflict(Name, LocalVars, Line),
    ensureNoNameConflict(Name, GlobalVars, Line),
    checkLabelConflict(Rest, GlobalVars, LocalVars);
checkLabelConflict([], _, _) ->
    ok.

-spec checkVariableConflict(variableTypeMap(), variableTypeMap()) -> ok.
checkVariableConflict(GlobalVars, LocalVars) ->
    case maps:to_list(maps:with(maps:keys(GlobalVars), LocalVars)) of
        [{Name, T} | _] ->
            throwNameConflict(Name, element(2, T));
        [] ->
            ok
    end.

-spec ensureNoNameConflict(atom(), variableTypeMap(), integer()) -> ok.
ensureNoNameConflict(Name, VarMap, Line) ->
    case maps:find(Name, VarMap) of
        {ok, _} ->
            throwNameConflict(Name, Line);
        _ ->
            ok
    end.

-spec throwNameConflict(atom(), integer()) -> no_return().
throwNameConflict(Name, Line) ->
    throw({Line, eUtil:fmt("name ~s has already been used", [Name])}).

-spec getValuesByDefinitions([#variableDefinition{}], #{atom() => any()}) -> [any()].
getValuesByDefinitions(DefList, Map) ->
    eUtil:getValuesByKeys(eUtil:namesOfVariableDefinitions(DefList), Map).

-spec variableDefinitionToReference([#variableDefinition{}]) -> [#variableReference{}].
variableDefinitionToReference(VariableDefinitions) ->
    lists:map(fun (#variableDefinition{name = N, line = Line}) -> #variableReference{name = N, line = Line} end, VariableDefinitions).
