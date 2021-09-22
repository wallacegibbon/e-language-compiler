%%% this is the 2nd pass, variable map will be created after this pass.
-module(ecompilerCollectVariable).

-export([fetchVariables/1]).

-include("ecompilerFrameDef.hrl").

-spec fetchVariables(eAST()) -> {eAST(), variableTypeMap(), eAST()}.
fetchVariables(AST) ->
    {Ast3, VarTypes, InitCode} = prvFetchVariables(prvPrepareStructInitExpression(AST), [], {#{}, [], true}),
    {Ast3, VarTypes, InitCode}.

-spec prvPrepareStructInitExpression(eAST()) -> eAST().
prvPrepareStructInitExpression([#function_raw{exprs = Expressions} = F | Rest]) ->
    [F#function_raw{exprs = prvFixStructInitAST(Expressions)} | prvPrepareStructInitExpression(Rest)];
prvPrepareStructInitExpression([#struct_raw{fields = Expressions} = S | Rest]) ->
    [S#struct_raw{fields = prvFixStructInitAST(Expressions)} | prvPrepareStructInitExpression(Rest)];
prvPrepareStructInitExpression([#vardef{initval = Initval} = V | Rest]) ->
    [V#vardef{initval = prvFixStructInit(Initval)} | prvPrepareStructInitExpression(Rest)];
prvPrepareStructInitExpression([]) ->
    [].

-spec prvFixStructInitAST(eAST()) -> eAST().
prvFixStructInitAST(Lst) ->
    ecompilerUtil:expressionMap(fun prvFixStructInit/1, Lst).

-spec prvFixStructInit(eExpression()) -> eExpression().
prvFixStructInit(#struct_init_raw{name = Name, fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = prvStructInitToMap(Fields),
    #struct_init{name = Name, field_names = FieldNames, field_values = InitExprMap, line = Line};
prvFixStructInit(#array_init{elements = Elements} = A) ->
    A#array_init{elements = prvFixStructInitAST(Elements)};
prvFixStructInit(#vardef{initval = Initval} = V) ->
    V#vardef{initval = prvFixStructInit(Initval)};
prvFixStructInit(#op2{op1 = Operand1, op2 = Operand2} = O) ->
    O#op2{op1 = prvFixStructInit(Operand1), op2 = prvFixStructInit(Operand2)};
prvFixStructInit(#op1{operand = Operand} = O) ->
    O#op1{operand = prvFixStructInit(Operand)};
prvFixStructInit(Any) ->
    Any.

-spec prvStructInitToMap([eExpression()]) -> {[#varref{}], #{atom() := eExpression()}}.
prvStructInitToMap(Expressions) ->
    prvStructInitToMap(Expressions, [], #{}).

-spec prvStructInitToMap([#op2{}], [#varref{}], #{atom() := eExpression()}) -> {[#varref{}], #{atom() := eExpression()}}.
prvStructInitToMap([#op2{operator = assign, op1 = #varref{name = Field} = Operand1, op2 = Val} | Rest], FieldNames, ExprMap) ->
    prvStructInitToMap(Rest, [Operand1 | FieldNames], ExprMap#{Field => prvFixStructInit(Val)});
prvStructInitToMap([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
-spec prvFetchVariables(eAST(), eAST(), {variableTypeMap(), eAST(), boolean()}) -> {eAST(), variableTypeMap(), eAST()}.
prvFetchVariables([#vardef{name = Name, type = Type, line = Line, initval = Initval} | Rest], NewAst, {VarTypes, InitCode, CollectInitCode}) ->
    prvEnsureNoNameConflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            prvFetchVariables(Rest, NewAst, {VarTypes#{Name => Type}, prvAppendToAST(InitCode, Name, Initval, Line), CollectInitCode});
        false ->
            prvFetchVariables(Rest, prvAppendToAST(NewAst, Name, Initval, Line), {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
prvFetchVariables([#function_raw{name = Name, ret = Ret, params = Params, exprs = Expressions, line = Line} | Rest], NewAst, {GlobalVars, _, _} = Ctx) ->
    {[], ParamVars, ParamInitCode} = prvFetchVariables(Params, [], {#{}, [], true}),
    ecompilerUtil:assert(ParamInitCode =:= [], {Line, "function parameters can not have default value"}),
    {NewExprs, FunVarTypes, []} = prvFetchVariables(Expressions, [], {ParamVars, [], false}),
    %% local variables should have different names from global variables
    prvCheckVariableConflict(GlobalVars, FunVarTypes),
    %% lable names should be different from variables, because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label end, Expressions),
    prvCheckLabelConflict(Labels, GlobalVars, FunVarTypes),
    FunctionType = #fun_type{params = prvGetValuesByDefinitions(Params, ParamVars), ret = Ret, line = Line},
    Function = #function{name = Name, var_types = FunVarTypes, exprs = NewExprs, param_names = prvVariableDefinitionToReference(Params), line = Line, type = FunctionType},
    prvFetchVariables(Rest, [Function | NewAst], Ctx);
prvFetchVariables([#struct_raw{name = Name, fields = Fields, line = Line} | Rest], NewAst, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = prvFetchVariables(Fields, [], {#{}, [], true}),
    {_, FieldInitMap} = prvStructInitToMap(StructInitCode),
    S = #struct{name = Name, field_types = FieldTypes, field_names = prvVariableDefinitionToReference(Fields), field_defaults = FieldInitMap, line = Line},
    prvFetchVariables(Rest, [S | NewAst], Ctx);
prvFetchVariables([Any | Rest], NewAst, Ctx) ->
    prvFetchVariables(Rest, [Any | NewAst], Ctx);
prvFetchVariables([], NewAst, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAst), VarTypes, lists:reverse(InitCode)}.

-spec prvAppendToAST(eAST(), atom(), eExpression(), integer()) -> eAST().
prvAppendToAST(AST, Varname, Initval, Line) when Initval =/= none ->
    [#op2{operator = assign, op1 = #varref{name = Varname, line = Line}, op2 = Initval, line = Line} | AST];
prvAppendToAST(AST, _, _, _) ->
    AST.

-spec prvCheckLabelConflict([eExpression()], variableTypeMap(), variableTypeMap()) -> ok.
prvCheckLabelConflict([#label{name = Name, line = Line} | Rest], GlobalVars, LocalVars) ->
    prvEnsureNoNameConflict(Name, LocalVars, Line),
    prvEnsureNoNameConflict(Name, GlobalVars, Line),
    prvCheckLabelConflict(Rest, GlobalVars, LocalVars);
prvCheckLabelConflict([], _, _) ->
    ok.

-spec prvCheckVariableConflict(variableTypeMap(), variableTypeMap()) -> ok.
prvCheckVariableConflict(GlobalVars, LocalVars) ->
    case maps:to_list(maps:with(maps:keys(GlobalVars), LocalVars)) of
        [{Name, T} | _] ->
            prvThrowNameConflict(Name, element(2, T));
        [] ->
            ok
    end.

-spec prvEnsureNoNameConflict(atom(), variableTypeMap(), integer()) -> ok.
prvEnsureNoNameConflict(Name, VarMap, Line) ->
    case maps:find(Name, VarMap) of
        {ok, _} ->
            prvThrowNameConflict(Name, Line);
        _ ->
            ok
    end.

-spec prvThrowNameConflict(atom(), integer()) -> no_return().
prvThrowNameConflict(Name, Line) ->
    throw({Line, ecompilerUtil:flatfmt("name ~s has already been used", [Name])}).

-spec prvGetValuesByDefinitions([#vardef{}], #{atom() => any()}) -> [any()].
prvGetValuesByDefinitions(DefList, Map) ->
    ecompilerUtil:getValuesByKeys(ecompilerUtil:namesOfVariableDefinitions(DefList), Map).

-spec prvVariableDefinitionToReference([#vardef{}]) -> [#varref{}].
prvVariableDefinitionToReference(Vardefs) ->
    lists:map(fun (#vardef{name = N, line = Line}) -> #varref{name = N, line = Line} end, Vardefs).
