%%% this is the 2nd pass, variable map will be created after this pass.
-module(ecompilerCollectVariable).

-export([fetchVariables/1]).

-include("ecompilerFrameDef.hrl").

-spec fetchVariables(eAST()) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables(AST) ->
    {Ast3, VarTypes, InitCode} = fetchVariables(prepareStructInitExpression(AST), [], {#{}, [], true}),
    {Ast3, VarTypes, InitCode}.

-spec prepareStructInitExpression(eAST()) -> eAST().
prepareStructInitExpression([#functionRaw{statements = Expressions} = F | Rest]) ->
    [F#functionRaw{statements = fixStructInitAST(Expressions)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([#structRaw{fields = Expressions} = S | Rest]) ->
    [S#structRaw{fields = fixStructInitAST(Expressions)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([#variableDefinition{initialValue = Initval} = V | Rest]) ->
    [V#variableDefinition{initialValue = fixStructInit(Initval)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([]) ->
    [].

-spec fixStructInitAST(eAST()) -> eAST().
fixStructInitAST(Lst) ->
    ecompilerUtil:expressionMap(fun fixStructInit/1, Lst).

-spec fixStructInit(eExpression()) -> eExpression().
fixStructInit(#structInitializeExpressionRaw{name = Name, fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = structInitToMap(Fields),
    #structInitializeExpression{name = Name, fieldNames = FieldNames, fieldValueMap = InitExprMap, line = Line};
fixStructInit(#arrayInitializeExpression{elements = Elements} = A) ->
    A#arrayInitializeExpression{elements = fixStructInitAST(Elements)};
fixStructInit(#variableDefinition{initialValue = Initval} = V) ->
    V#variableDefinition{initialValue = fixStructInit(Initval)};
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

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
-spec fetchVariables(eAST(), eAST(), {variableTypeMap(), eAST(), boolean()}) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables([#variableDefinition{name = Name, type = Type, line = Line, initialValue = Initval} | Rest], NewAst, {VarTypes, InitCode, CollectInitCode}) ->
    ensureNoNameConflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            fetchVariables(Rest, NewAst, {VarTypes#{Name => Type}, appendToAST(InitCode, Name, Initval, Line), CollectInitCode});
        false ->
            fetchVariables(Rest, appendToAST(NewAst, Name, Initval, Line), {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
fetchVariables([#functionRaw{name = Name, returnType = Ret, parameters = Params, statements = Expressions, line = Line} | Rest], NewAst, {GlobalVars, _, _} = Ctx) ->
    {[], ParamVars, ParamInitCode} = fetchVariables(Params, [], {#{}, [], true}),
    ecompilerUtil:assert(ParamInitCode =:= [], {Line, "function parameters can not have default value"}),
    {NewExprs, FunVarTypes, []} = fetchVariables(Expressions, [], {ParamVars, [], false}),
    %% local variables should have different names from global variables
    checkVariableConflict(GlobalVars, FunVarTypes),
    %% lable names should be different from variables, because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label end, Expressions),
    checkLabelConflict(Labels, GlobalVars, FunVarTypes),
    FunctionType = #functionType{parameters = getValuesByDefinitions(Params, ParamVars), ret = Ret, line = Line},
    Function = #function{name = Name, variableTypeMap = FunVarTypes, statements = NewExprs, parameterNames = variableDefinitionToReference(Params), line = Line, type = FunctionType},
    fetchVariables(Rest, [Function | NewAst], Ctx);
fetchVariables([#structRaw{name = Name, fields = Fields, line = Line} | Rest], NewAst, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetchVariables(Fields, [], {#{}, [], true}),
    {_, FieldInitMap} = structInitToMap(StructInitCode),
    S = #struct{name = Name, fieldTypeMap = FieldTypes, fieldNames = variableDefinitionToReference(Fields), fieldDefaultValueMap = FieldInitMap, line = Line},
    fetchVariables(Rest, [S | NewAst], Ctx);
fetchVariables([Any | Rest], NewAst, Ctx) ->
    fetchVariables(Rest, [Any | NewAst], Ctx);
fetchVariables([], NewAst, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAst), VarTypes, lists:reverse(InitCode)}.

-spec appendToAST(eAST(), atom(), eExpression(), integer()) -> eAST().
appendToAST(AST, Varname, Initval, Line) when Initval =/= none ->
    [#operatorExpression2{operator = assign, operand1 = #variableReference{name = Varname, line = Line}, operand2 = Initval, line = Line} | AST];
appendToAST(AST, _, _, _) ->
    AST.

-spec checkLabelConflict([eExpression()], variableTypeMap(), variableTypeMap()) -> ok.
checkLabelConflict([#label{name = Name, line = Line} | Rest], GlobalVars, LocalVars) ->
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
    throw({Line, ecompilerUtil:fmt("name ~s has already been used", [Name])}).

-spec getValuesByDefinitions([#variableDefinition{}], #{atom() => any()}) -> [any()].
getValuesByDefinitions(DefList, Map) ->
    ecompilerUtil:getValuesByKeys(ecompilerUtil:namesOfVariableDefinitions(DefList), Map).

-spec variableDefinitionToReference([#variableDefinition{}]) -> [#variableReference{}].
variableDefinitionToReference(Vardefs) ->
    lists:map(fun (#variableDefinition{name = N, line = Line}) -> #variableReference{name = N, line = Line} end, Vardefs).
