%%% this is the 2nd pass, variable map will be created after this pass.
-module(ecompilerCollectVariable).

-export([fetchVariables/1]).

-include("ecompilerFrameDef.hrl").

-spec fetchVariables(eAST()) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables(AST) ->
    {Ast3, VarTypes, InitCode} = fetchVariables(prepareStructInitExpression(AST), [], {#{}, [], true}),
    {Ast3, VarTypes, InitCode}.

-spec prepareStructInitExpression(eAST()) -> eAST().
prepareStructInitExpression([#function_raw{exprs = Expressions} = F | Rest]) ->
    [F#function_raw{exprs = fixStructInitAST(Expressions)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([#struct_raw{fields = Expressions} = S | Rest]) ->
    [S#struct_raw{fields = fixStructInitAST(Expressions)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([#vardef{initval = Initval} = V | Rest]) ->
    [V#vardef{initval = fixStructInit(Initval)} | prepareStructInitExpression(Rest)];
prepareStructInitExpression([]) ->
    [].

-spec fixStructInitAST(eAST()) -> eAST().
fixStructInitAST(Lst) ->
    ecompilerUtil:expressionMap(fun fixStructInit/1, Lst).

-spec fixStructInit(eExpression()) -> eExpression().
fixStructInit(#struct_init_raw{name = Name, fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = structInitToMap(Fields),
    #struct_init{name = Name, field_names = FieldNames, field_values = InitExprMap, line = Line};
fixStructInit(#array_init{elements = Elements} = A) ->
    A#array_init{elements = fixStructInitAST(Elements)};
fixStructInit(#vardef{initval = Initval} = V) ->
    V#vardef{initval = fixStructInit(Initval)};
fixStructInit(#op2{op1 = Operand1, op2 = Operand2} = O) ->
    O#op2{op1 = fixStructInit(Operand1), op2 = fixStructInit(Operand2)};
fixStructInit(#op1{operand = Operand} = O) ->
    O#op1{operand = fixStructInit(Operand)};
fixStructInit(Any) ->
    Any.

-spec structInitToMap([eExpression()]) -> {[#varref{}], #{atom() := eExpression()}}.
structInitToMap(Expressions) ->
    structInitToMap(Expressions, [], #{}).

-spec structInitToMap([#op2{}], [#varref{}], #{atom() := eExpression()}) -> {[#varref{}], #{atom() := eExpression()}}.
structInitToMap([#op2{operator = assign, op1 = #varref{name = Field} = Operand1, op2 = Val} | Rest], FieldNames, ExprMap) ->
    structInitToMap(Rest, [Operand1 | FieldNames], ExprMap#{Field => fixStructInit(Val)});
structInitToMap([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
-spec fetchVariables(eAST(), eAST(), {variableTypeMap(), eAST(), boolean()}) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables([#vardef{name = Name, type = Type, line = Line, initval = Initval} | Rest], NewAst, {VarTypes, InitCode, CollectInitCode}) ->
    ensureNoNameConflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            fetchVariables(Rest, NewAst, {VarTypes#{Name => Type}, appendToAST(InitCode, Name, Initval, Line), CollectInitCode});
        false ->
            fetchVariables(Rest, appendToAST(NewAst, Name, Initval, Line), {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
fetchVariables([#function_raw{name = Name, ret = Ret, params = Params, exprs = Expressions, line = Line} | Rest], NewAst, {GlobalVars, _, _} = Ctx) ->
    {[], ParamVars, ParamInitCode} = fetchVariables(Params, [], {#{}, [], true}),
    ecompilerUtil:assert(ParamInitCode =:= [], {Line, "function parameters can not have default value"}),
    {NewExprs, FunVarTypes, []} = fetchVariables(Expressions, [], {ParamVars, [], false}),
    %% local variables should have different names from global variables
    checkVariableConflict(GlobalVars, FunVarTypes),
    %% lable names should be different from variables, because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label end, Expressions),
    checkLabelConflict(Labels, GlobalVars, FunVarTypes),
    FunctionType = #fun_type{params = getValuesByDefinitions(Params, ParamVars), ret = Ret, line = Line},
    Function = #function{name = Name, var_types = FunVarTypes, exprs = NewExprs, param_names = variableDefinitionToReference(Params), line = Line, type = FunctionType},
    fetchVariables(Rest, [Function | NewAst], Ctx);
fetchVariables([#struct_raw{name = Name, fields = Fields, line = Line} | Rest], NewAst, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetchVariables(Fields, [], {#{}, [], true}),
    {_, FieldInitMap} = structInitToMap(StructInitCode),
    S = #struct{name = Name, field_types = FieldTypes, field_names = variableDefinitionToReference(Fields), field_defaults = FieldInitMap, line = Line},
    fetchVariables(Rest, [S | NewAst], Ctx);
fetchVariables([Any | Rest], NewAst, Ctx) ->
    fetchVariables(Rest, [Any | NewAst], Ctx);
fetchVariables([], NewAst, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAst), VarTypes, lists:reverse(InitCode)}.

-spec appendToAST(eAST(), atom(), eExpression(), integer()) -> eAST().
appendToAST(AST, Varname, Initval, Line) when Initval =/= none ->
    [#op2{operator = assign, op1 = #varref{name = Varname, line = Line}, op2 = Initval, line = Line} | AST];
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
    throw({Line, ecompilerUtil:flatfmt("name ~s has already been used", [Name])}).

-spec getValuesByDefinitions([#vardef{}], #{atom() => any()}) -> [any()].
getValuesByDefinitions(DefList, Map) ->
    ecompilerUtil:getValuesByKeys(ecompilerUtil:namesOfVariableDefinitions(DefList), Map).

-spec variableDefinitionToReference([#vardef{}]) -> [#varref{}].
variableDefinitionToReference(Vardefs) ->
    lists:map(fun (#vardef{name = N, line = Line}) -> #varref{name = N, line = Line} end, Vardefs).
