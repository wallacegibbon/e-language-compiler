-module(e_variable).
-export([fetch_variables/1]).

-include("e_record_definition.hrl").

-spec fetch_variables(e_ast()) -> {e_ast(), var_type_map(), e_ast()}.
fetch_variables(AST) ->
    {AST3, VarTypes, InitCode} = fetch_variables(prepare_struct_init_expr(AST), [], {#{}, [], true}),
    {AST3, VarTypes, InitCode}.

-spec prepare_struct_init_expr(e_ast()) -> e_ast().
prepare_struct_init_expr([#function_raw{stmts = Exprs} = F | Rest]) ->
    [F#function_raw{stmts = fix_struct_init_ast(Exprs)} | prepare_struct_init_expr(Rest)];
prepare_struct_init_expr([#struct_raw{fields = Exprs} = S | Rest]) ->
    [S#struct_raw{fields = fix_struct_init_ast(Exprs)} | prepare_struct_init_expr(Rest)];
prepare_struct_init_expr([#var_def{init_value = Value} = V | Rest]) ->
    [V#var_def{init_value = fix_struct_init(Value)} | prepare_struct_init_expr(Rest)];
prepare_struct_init_expr([]) ->
    [].

-spec fix_struct_init_ast(e_ast()) -> e_ast().
fix_struct_init_ast(Lst) ->
    e_util:expr_map(fun fix_struct_init/1, Lst).

-spec fix_struct_init(e_expr()) -> e_expr().
fix_struct_init(#struct_init_raw_expr{name = Name, fields = Fields, line = Line}) ->
    {FieldNames, InitExprMap} = struct_init_to_map(Fields),
    #struct_init_expr{name = Name, field_names = FieldNames, field_value_map = InitExprMap, line = Line};
fix_struct_init(#array_init_expr{elements = Elements} = A) ->
    A#array_init_expr{elements = fix_struct_init_ast(Elements)};
fix_struct_init(#var_def{init_value = InitialValue} = V) ->
    V#var_def{init_value = fix_struct_init(InitialValue)};
fix_struct_init(#op2_expr{operand1 = Op1, operand2 = Op2} = O) ->
    O#op2_expr{operand1 = fix_struct_init(Op1), operand2 = fix_struct_init(Op2)};
fix_struct_init(#op1_expr{operand = Operand} = O) ->
    O#op1_expr{operand = fix_struct_init(Operand)};
fix_struct_init(Any) ->
    Any.

-spec struct_init_to_map([e_expr()]) -> {[#var_ref{}], #{atom() := e_expr()}}.
struct_init_to_map(Exprs) ->
    struct_init_to_map(Exprs, [], #{}).

-spec struct_init_to_map([#op2_expr{}], [#var_ref{}], #{atom() := e_expr()}) -> {[#var_ref{}], #{atom() := e_expr()}}.
struct_init_to_map([#op2_expr{operator = assign, operand1 = #var_ref{name = Field} = Op1, operand2 = Val} | Rest],
                   FieldNames, ExprMap) ->
    struct_init_to_map(Rest, [Op1 | FieldNames], ExprMap#{Field => fix_struct_init(Val)});
struct_init_to_map([], FieldNames, ExprMap) ->
    {FieldNames, ExprMap}.

%% In function expressions, the init code of defvar can not be simply fetched out from the code,
%% it should be replaced as assignment in the same place.
-spec fetch_variables(e_ast(), e_ast(), {var_type_map(), e_ast(), boolean()}) ->
          {e_ast(), var_type_map(), e_ast()}.
fetch_variables([#var_def{name = Name, type = Type, line = Line, init_value = InitialValue} | Rest],
                NewAST, {VarTypes, InitCode, CollectInitCode}) ->
    ensure_no_name_conflict(Name, VarTypes, Line),
    case CollectInitCode of
        true ->
            fetch_variables(Rest, NewAST,
                            {VarTypes#{Name => Type}, append_to_ast(InitCode, Name, InitialValue, Line), CollectInitCode});
        false ->
            fetch_variables(Rest, append_to_ast(NewAST, Name, InitialValue, Line),
                            {VarTypes#{Name => Type}, InitCode, CollectInitCode})
    end;
fetch_variables([#function_raw{name = Name, ret_type = Ret, params = Params, stmts = Exprs, line = Line} | Rest],
                NewAST, {GlobalVars, _, _} = Ctx) ->
    {[], ParamVars, ParamInitCode} = fetch_variables(Params, [], {#{}, [], true}),
    e_util:assert(ParamInitCode =:= [], {Line, "function params can not have default value"}),
    {NewExprs, FnVarTypes, []} = fetch_variables(Exprs, [], {ParamVars, [], false}),
    %% local variables should have different names from global variables
    check_variable_conflict(GlobalVars, FnVarTypes),
    %% label names should be different from variables, because the operand of goto could be a pointer variable.
    Labels = lists:filter(fun (E) -> element(1, E) =:= label end, Exprs),
    check_label_conflict(Labels, GlobalVars, FnVarTypes),
    FnType = #fn_type{params = get_values_by_defs(Params, ParamVars), ret = Ret, line = Line},
    Function = #function{name = Name, var_type_map = FnVarTypes, stmts = NewExprs,
                         param_names = var_defs_to_refs(Params), line = Line, type = FnType},
    fetch_variables(Rest, [Function | NewAST], Ctx);
fetch_variables([#struct_raw{name = Name, fields = Fields, line = Line} | Rest], NewAST, Ctx) ->
    %% struct can have default value
    {[], FieldTypes, StructInitCode} = fetch_variables(Fields, [], {#{}, [], true}),
    {_, FieldInitMap} = struct_init_to_map(StructInitCode),
    S = #struct{name = Name, field_type_map = FieldTypes, field_names = var_defs_to_refs(Fields), field_default_value_map = FieldInitMap, line = Line},
    fetch_variables(Rest, [S | NewAST], Ctx);
fetch_variables([Any | Rest], NewAST, Ctx) ->
    fetch_variables(Rest, [Any | NewAST], Ctx);
fetch_variables([], NewAST, {VarTypes, InitCode, _}) ->
    {lists:reverse(NewAST), VarTypes, lists:reverse(InitCode)}.

-spec append_to_ast(e_ast(), atom(), e_expr(), integer()) -> e_ast().
append_to_ast(AST, VarName, InitialValue, Line) when InitialValue =/= none ->
    [#op2_expr{operator = assign, operand1 = #var_ref{name = VarName, line = Line}, operand2 = InitialValue, line = Line} | AST];
append_to_ast(AST, _, _, _) ->
    AST.

-spec check_label_conflict([e_expr()], var_type_map(), var_type_map()) -> ok.
check_label_conflict([#goto_label{name = Name, line = Line} | Rest], GlobalVars, LocalVars) ->
    ensure_no_name_conflict(Name, LocalVars, Line),
    ensure_no_name_conflict(Name, GlobalVars, Line),
    check_label_conflict(Rest, GlobalVars, LocalVars);
check_label_conflict([], _, _) ->
    ok.

-spec check_variable_conflict(var_type_map(), var_type_map()) -> ok.
check_variable_conflict(GlobalVars, LocalVars) ->
    case maps:to_list(maps:with(maps:keys(GlobalVars), LocalVars)) of
        [{Name, T} | _] ->
            throw_name_conflict(Name, element(2, T));
        [] ->
            ok
    end.

-spec ensure_no_name_conflict(atom(), var_type_map(), integer()) -> ok.
ensure_no_name_conflict(Name, VarMap, Line) ->
    case maps:find(Name, VarMap) of
        {ok, _} ->
            throw_name_conflict(Name, Line);
        _ ->
            ok
    end.

-spec throw_name_conflict(atom(), integer()) -> no_return().
throw_name_conflict(Name, Line) ->
    throw({Line, e_util:fmt("name ~s has already been used", [Name])}).

-spec get_values_by_defs([#var_def{}], #{atom() => any()}) -> [any()].
get_values_by_defs(DefList, Map) ->
    e_util:get_values_by_keys(e_util:names_of_var_defs(DefList), Map).

-spec var_defs_to_refs([#var_def{}]) -> [#var_ref{}].
var_defs_to_refs(VarDefList) ->
    lists:map(fun (#var_def{name = N, line = Line}) -> #var_ref{name = N, line = Line} end, VarDefList).