%%% this is the 2nd pass, variable map will be created after this pass.
-module(ecompilerCollectVariable).

-export([fetchVariables/1]).

-include("./ecompilerFrameDef.hrl").

fetchVariables(Ast) ->
    Ast2 = prvPrepareStructInitExpression(Ast),
    {Ast3, VarTypes, InitCode} = prvFetchVariables(Ast2, [], {#{}, [], true}),
    {Ast3, VarTypes, InitCode}.

prvPrepareStructInitExpression([#function_raw{exprs = Exprs} = F | Rest]) ->
    [F#function_raw{exprs = prvFixStructInitAST(Exprs)} | prvPrepareStructInitExpression(Rest)];
prvPrepareStructInitExpression([#struct_raw{fields = Exprs} = S | Rest]) ->
    [S#struct_raw{fields = prvFixStructInitAST(Exprs)} | prvPrepareStructInitExpression(Rest)];
prvPrepareStructInitExpression([#vardef{initval = Initval} = V | Rest]) ->
    [V#vardef{initval = prvFixStructInit(Initval)} | prvPrepareStructInitExpression(Rest)];
prvPrepareStructInitExpression([]) ->
    [].

prvFixStructInitAST(Lst) -> ecompilerUtil:expressionMap(fun prvFixStructInit/1, Lst).

prvFixStructInit(#struct_init_raw{name = Name, fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = prvStructInitToMap(Fields),
    #struct_init{name = Name, field_names = FieldNames, field_values = InitExprMap, line = Line};
prvFixStructInit(#array_init{elements = Elements} = A) ->
    A#array_init{elements = prvFixStructInitAST(Elements)};
prvFixStructInit(#vardef{initval = Initval} = V) ->
    V#vardef{initval = prvFixStructInit(Initval)};
prvFixStructInit(#op2{op1 = Op1, op2 = Op2} = O) ->
    O#op2{op1 = prvFixStructInit(Op1), op2 = prvFixStructInit(Op2)};
prvFixStructInit(#op1{operand = Operand} = O) ->
    O#op1{operand = prvFixStructInit(Operand)};
prvFixStructInit(Any) ->
    Any.

-define(ASSIGN_OP(Op1, Op2), #op2{operator = assign, op1 = Op1, op2 = Op2}).

prvStructInitToMap(Exprs) -> prvStructInitToMap(Exprs, [], #{}).

prvStructInitToMap([?ASSIGN_OP((#varref{name = Field} = Op1), Val) | Rest], FieldNames, ExprMap) ->
    prvStructInitToMap(Rest, [Op1 | FieldNames], ExprMap#{Field => prvFixStructInit(Val)});
prvStructInitToMap([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% in function expressions, the init code of defvar can not be simply
%% fetched out from the code, it should be replaced as assignment in the
%% same place.
prvFetchVariables([#vardef{name = Name, type = Type, line = Line, initval = Initval} | Rest], NewAst, {VarTypes, InitCode, CollectInitCode}) ->
    prvEnsureNoNameConflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            prvFetchVariables(Rest, NewAst, {VarTypes#{Name => Type}, prvAppendToAST(InitCode, Name, Initval, Line), CollectInitCode});
        _ ->
            prvFetchVariables(Rest, prvAppendToAST(NewAst, Name, Initval, Line), {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
prvFetchVariables([#function_raw{name = Name, ret = Ret, params = Params, exprs = Exprs, line = Line} | Rest], NewAst, {GlobalVars, _, _} = Ctx) ->
    {[], ParamVars, ParamInitCode} = prvFetchVariables(Params, [], {#{}, [], true}),
    ecompilerUtil:assert(ParamInitCode =:= [], {Line, "function parameters can not have default value"}),
    {NewExprs, FunVarTypes, []} = prvFetchVariables(Exprs, [], {ParamVars, [], false}),
    %% local variables should have different names from global variables
    prvCheckVariableConflict(GlobalVars, FunVarTypes),
    %% lable names should be different from variables,
    %% because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label end, Exprs),
    prvCheckLabelConflict(Labels, GlobalVars, FunVarTypes),
    ParamsForType = prvGetValuesByDefinitions(Params, ParamVars),
    Fn = #function{name = Name, var_types = FunVarTypes, exprs = NewExprs, param_names = prvVariableDefinitionToReference(Params), line = Line,
                   type = #fun_type{params = ParamsForType, ret = Ret, line = Line}},
    prvFetchVariables(Rest, [Fn | NewAst], Ctx);
prvFetchVariables([#struct_raw{name = Name, fields = Fields, line = Line} | Rest], NewAst, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = prvFetchVariables(Fields, [], {#{}, [], true}),
    {_, FieldInitMap} = prvStructInitToMap(StructInitCode),
    FieldNames = prvVariableDefinitionToReference(Fields),
    S = #struct{name = Name, field_types = FieldTypes, field_names = FieldNames, field_defaults = FieldInitMap, line = Line},
    prvFetchVariables(Rest, [S | NewAst], Ctx);
prvFetchVariables([Any | Rest], NewAst, Ctx) ->
    prvFetchVariables(Rest, [Any | NewAst], Ctx);
prvFetchVariables([], NewAst, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAst), VarTypes, lists:reverse(InitCode)}.

prvAppendToAST(Ast, Varname, Initval, Line) when Initval =/= none ->
    [#op2{operator = assign, op1 = #varref{name = Varname, line = Line}, op2 = Initval, line = Line} | Ast];
prvAppendToAST(Ast, _, _, _) ->
    Ast.

prvCheckLabelConflict([#label{name = Name, line = Line} | Rest], GlobalVars, LocalVars) ->
    prvEnsureNoNameConflict(Name, LocalVars, Line),
    prvEnsureNoNameConflict(Name, GlobalVars, Line),
    prvCheckLabelConflict(Rest, GlobalVars, LocalVars);
prvCheckLabelConflict([], _, _) ->
    ok.

prvCheckVariableConflict(GlobalVars, LocalVars) ->
    ConflictMap = maps:with(maps:keys(GlobalVars), LocalVars),
    maps:map(fun (Name, T) -> prvThrowNameConflict(Name, element(2, T)) end, ConflictMap).

prvEnsureNoNameConflict(Name, VarMap, Line) ->
    case maps:find(Name, VarMap) of
        {ok, _} ->
            prvThrowNameConflict(Name, Line);
        _ ->
            ok
    end.

prvThrowNameConflict(Name, Line) ->
    throw({Line, ecompilerUtil:flatfmt("name ~s has already been used", [Name])}).

prvGetValuesByDefinitions(DefList, Map) ->
    ecompilerUtil:getValuesByKeys(ecompilerUtil:namesOfVariableDefinitiions(DefList), Map).

prvVariableDefinitionToReference(Vardefs) ->
    lists:map(fun (#vardef{name = N, line = Line}) -> #varref{name = N, line = Line} end, Vardefs).

