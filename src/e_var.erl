-module(e_var).
-export([fetch_vars/1, shift_offset/2, shift_offset_middle/1]).
-include("e_record_definition.hrl").

-type fetch_vars_state() :: #{
                              vars := #e_vars{},
                              names := [atom()],
                              initcode := e_ast(),
                              mode := normal | initcode,
                              tag := e_var_type()
                             }.

fetch_vars_state_new() ->
    #{vars => #e_vars{}, names => [], initcode => [], mode => normal, tag => none}.

-spec fetch_vars(e_ast_raw()) -> {#e_vars{}, e_ast_raw(), e_ast_raw()}.
fetch_vars(AST) ->
    Ctx = fetch_vars_state_new(),
    fetch_vars(prepare_struct_init_expr(AST), [], Ctx#{tag := global, mode := initcode}).

-spec prepare_struct_init_expr(e_ast_raw()) -> e_ast_raw().
prepare_struct_init_expr([#e_function_raw{stmts = Stmts} = Fn | Rest]) ->
    [Fn#e_function_raw{stmts = fix_struct_init_expr_in_stmts(Stmts)} | prepare_struct_init_expr(Rest)];
prepare_struct_init_expr([#e_struct_raw{fields = Stmts} = S | Rest]) ->
    [S#e_struct_raw{fields = fix_struct_init_expr_in_stmts(Stmts)} | prepare_struct_init_expr(Rest)];
prepare_struct_init_expr([#e_vardef{init_value = Value} = V | Rest]) ->
    [V#e_vardef{init_value = fix_struct_init(Value)} | prepare_struct_init_expr(Rest)];
prepare_struct_init_expr([]) ->
    [].

-spec fix_struct_init_expr_in_stmts([e_stmt()]) -> [e_stmt()].
fix_struct_init_expr_in_stmts(List) ->
    e_util:expr_map(fun fix_struct_init/1, List).

-spec fix_struct_init(e_stmt()) -> e_stmt().
fix_struct_init(#e_struct_init_raw_expr{name = Name, fields = Fields, loc = Loc}) ->
    #e_struct_init_expr{name = Name, field_value_map = struct_init_to_map(Fields, #{}), loc = Loc};
fix_struct_init(#e_array_init_expr{elements = Elements} = A) ->
    A#e_array_init_expr{elements = [fix_struct_init(E) || E <- Elements]};
fix_struct_init(#e_vardef{init_value = InitialValue} = V) ->
    V#e_vardef{init_value = fix_struct_init(InitialValue)};
fix_struct_init(?CALL(Fn, Args) = O) ->
    O?CALL(fix_struct_init(Fn), [fix_struct_init(A) || A <- Args]);
fix_struct_init(#e_op{data = Operands} = O) ->
    O#e_op{data = [fix_struct_init(A) || A <- Operands]};
fix_struct_init(#e_type_convert{expr = Expr} = C) ->
    C#e_type_convert{expr = fix_struct_init(Expr)};
fix_struct_init(Any) ->
    Any.

-spec struct_init_to_map([e_expr()], #{atom() => e_expr()}) -> #{atom() => e_expr()}.
struct_init_to_map([?OP2('=', ?VREF(Field), Val) | Rest], ExprMap) ->
    struct_init_to_map(Rest, ExprMap#{Field => fix_struct_init(Val)});
struct_init_to_map([], ExprMap) ->
    ExprMap.

%% In function expressions, the init code of `vardef`s can not be simply fetched out from the code,
%% they should be replaced as assignment in the same place.

%% CAUTION!: fetch_vars/3 generate IN-COMPLETE `#e_vars{}` values here.
%%  (For functions, structs and global variables)
%%
%% Only `names`, `type_map` and `tag` fields are updated.
%% `size`, `align` and `offset_map` fields are to be updated by functions in `e_size.erl`.

-spec fetch_vars(e_ast_raw(), e_ast_raw(), fetch_vars_state()) -> {#e_vars{}, e_ast(), e_ast()}.
fetch_vars([#e_vardef{name = Name, type = Type, loc = Loc, init_value = InitialValue} | Rest],
           AST, #{mode := initcode} = Ctx) ->
    #{vars := #e_vars{type_map = TypeMap} = Vars, names := Names, initcode := InitCode} = Ctx,
    check_name_conflict(Name, Vars, Loc),
    Vars1 = Vars#e_vars{type_map = TypeMap#{Name => Type}},
    InitCode1 = append_to_ast(InitCode, Name, InitialValue, Loc),
    Ctx1 = Ctx#{vars := Vars1, names := [Name | Names], initcode := InitCode1},
    fetch_vars(Rest, AST, Ctx1);
fetch_vars([#e_vardef{name = Name, type = Type, loc = Loc, init_value = InitialValue} | Rest],
           AST, Ctx) ->
    #{vars := #e_vars{type_map = TypeMap} = Vars, names := Names} = Ctx,
    check_name_conflict(Name, Vars, Loc),
    Vars1 = Vars#e_vars{type_map = TypeMap#{Name => Type}},
    Ctx1 = Ctx#{vars := Vars1, names := [Name | Names]},
    fetch_vars(Rest, append_to_ast(AST, Name, InitialValue, Loc), Ctx1);
fetch_vars([#e_function_raw{name = Name, ret_type = Ret, params = Params, stmts = Stmts,
                            attribute = Attr, loc = Loc} | Rest],
           AST, Ctx) ->
    #{vars := GlobalVars} = Ctx,
    Ctx1 = fetch_vars_state_new(),
    {ParamVars, [], ParamInitCode} = fetch_vars(Params, [], Ctx1#{tag := local}),
    e_util:assert(ParamInitCode =:= [], {Loc, "function params can not have default value"}),
    check_variable_conflict(GlobalVars, ParamVars),
    {LocalVars, NewStmts, []} = fetch_vars(Stmts, [], Ctx1#{tag := local, vars := ParamVars}),
    check_variable_conflict(GlobalVars, LocalVars),
    FnType = #e_fn_type{params = get_values_by_defs(Params, ParamVars), ret = Ret, loc = Loc},
    ParamNames = e_util:names_of_var_defs(Params),
    check_label_conflict(NewStmts, #{}),
    Fn = #e_function{name = Name, vars = LocalVars, param_names = ParamNames, type = FnType,
                     stmts = NewStmts, loc = Loc, attribute = Attr},
    fetch_vars(Rest, [Fn | AST], Ctx);
fetch_vars([#e_struct_raw{name = Name, fields = RawFields, loc = Loc} | Rest], AST, Ctx) ->
    %% struct can have default value
    Ctx1 = fetch_vars_state_new(),
    {Fields, [], StructInitCode} = fetch_vars(RawFields, [], Ctx1#{tag := none, mode := initcode}),
    %% Default value for struct can caused some bugs which I don't have time to fix.
    e_util:assert(StructInitCode =:= [],
                  {Loc, "default value for struct is not supported yet"}),
    FieldInitMap = struct_init_to_map(StructInitCode, #{}),
    S = #e_struct{name = Name, fields = Fields, default_value_map = FieldInitMap, loc = Loc},
    fetch_vars(Rest, [S | AST], Ctx);
fetch_vars([Any | Rest], AST, Ctx) ->
    fetch_vars(Rest, [Any | AST], Ctx);
fetch_vars([], AST, #{vars := Vars0, names := Names, initcode := InitCode, tag := Tag}) ->
    #e_vars{names = OldNames} = Vars0,
    Vars1 = Vars0#e_vars{names = OldNames ++ lists:reverse(Names), tag = Tag},
    {Vars1, lists:reverse(AST), lists:reverse(InitCode)}.

-spec append_to_ast([e_stmt()], atom(), e_expr(), location()) -> e_ast().
append_to_ast(AST, VarName, InitialValue, Loc) when InitialValue =/= none ->
    [?OP2('=', ?VREF(VarName, Loc), InitialValue, Loc) | AST];
append_to_ast(AST, _, _, _) ->
    AST.

-spec check_variable_conflict(#e_vars{}, #e_vars{}) -> ok.
check_variable_conflict(#e_vars{type_map = GlobalVarMap}, #e_vars{type_map = LocalVarMap}) ->
    case maps:to_list(maps:with(maps:keys(GlobalVarMap), LocalVarMap)) of
        [{Name, T} | _] ->
            e_util:ethrow(element(2, T), "name \"~s\" has already been used", [Name]);
        [] ->
            ok
    end.

-spec check_name_conflict(atom(), #e_vars{}, location()) -> ok.
check_name_conflict(Name, #e_vars{type_map = VarMap}, Loc) ->
    case maps:find(Name, VarMap) of
        {ok, _} ->
            e_util:ethrow(Loc, "name \"~s\" has already been used", [Name]);
        _ ->
            ok
    end.

-spec check_label_conflict([e_stmt()], #{atom() => location()}) -> ok.
check_label_conflict([#e_label{name = Name, loc = Loc} | Rest], Map) ->
    case maps:find(Name, Map) of
        {ok, {Line, Col}} ->
            e_util:ethrow(Loc, "label \"~s\" conflict with line ~w:~w", [Name, {Line, Col}]);
        error ->
            check_label_conflict(Rest, Map#{Name => Loc})
    end;
check_label_conflict([_ | Rest], Map) ->
    check_label_conflict(Rest, Map);
check_label_conflict([], _) ->
    ok.

-spec get_values_by_defs([#e_vardef{}], #e_vars{}) -> [any()].
get_values_by_defs(DefList, #e_vars{type_map = Map}) ->
    e_util:get_values_by_keys(e_util:names_of_var_defs(DefList), Map).

-spec shift_offset(#e_vars{}, non_neg_integer()) -> #e_vars{}.
shift_offset(#e_vars{offset_map = OffsetMap, size = Size} = Vars, Num) ->
    OffsetMapNew = #{K => {O - Num, S} || K := {O, S} <- OffsetMap},
    Vars#e_vars{offset_map = OffsetMapNew, shifted_size = Size - Num}.

-spec shift_offset_middle(#e_vars{}) -> #e_vars{}.
shift_offset_middle(#e_vars{size = Size} = Vars) ->
    shift_offset(Vars, Size div 2).
