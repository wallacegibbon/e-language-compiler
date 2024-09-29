-module(e_var).
-export([fetch_vars/1, shift_offset/2, shift_offset_middle/1]).
-include("e_record_definition.hrl").

-spec fetch_vars(e_ast_raw()) -> {#e_vars{}, e_ast_raw(), e_ast_raw()}.
fetch_vars(AST) ->
	fetch_vars(prepare_struct_init_expr(AST), [], {#e_vars{}, [], [], true, global}).

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
	A#e_array_init_expr{elements = lists:map(fun fix_struct_init/1, Elements)};
fix_struct_init(#e_vardef{init_value = InitialValue} = V) ->
	V#e_vardef{init_value = fix_struct_init(InitialValue)};
fix_struct_init(?CALL(Callee, Args) = O) ->
	O?CALL(fix_struct_init(Callee), lists:map(fun fix_struct_init/1, Args));
fix_struct_init(#e_op{data = Operands} = O) ->
	O#e_op{data = lists:map(fun fix_struct_init/1, Operands)};
fix_struct_init(#e_type_convert{expr = Expr} = C) ->
	C#e_type_convert{expr = fix_struct_init(Expr)};
fix_struct_init(Any) ->
	Any.

-spec struct_init_to_map([e_expr()], #{atom() => e_expr()}) -> #{atom() => e_expr()}.
struct_init_to_map([?OP2('=', #e_varref{name = Field}, Val) | Rest], ExprMap) ->
	struct_init_to_map(Rest, ExprMap#{Field => fix_struct_init(Val)});
struct_init_to_map([], ExprMap) ->
	ExprMap.

%% In function expressions, the init code of vardef can not be simply fetched out from the code,
%% it should be replaced as assignment in the same place.

-type fetch_vars_state() ::
	{
	Vars ::#e_vars{},
	Names :: [atom()],
	AST :: e_ast(),
	CollectInitCode :: boolean(),
	Tag :: e_var_type()
	}.

%% Caution: fetch_vars/3 generate IN-COMPLETE `#e_vars{}` values here. (For functions, structs and global variables)
%% Only `names`, `type_map` and `tag` fields are updated.
%% `size`, `align` and `offset_map` fields are to be updated by functions in `e_size.erl`.

-spec fetch_vars(e_ast_raw(), e_ast_raw(), fetch_vars_state()) -> {#e_vars{}, e_ast(), e_ast()}.
fetch_vars([#e_vardef{} = Hd | Rest], AST, {#e_vars{type_map = TypeMap} = Vars, Names, InitCode, true, Tag}) ->
	#e_vardef{name = Name, type = Type, loc = Loc, init_value = InitialValue} = Hd,
	check_name_conflict(Name, Vars, Loc),
	NewInitCode = append_to_ast(InitCode, Name, InitialValue, Loc),
	NewVars = Vars#e_vars{type_map = TypeMap#{Name => Type}},
	NewCtx = {NewVars, [Name | Names], NewInitCode, true, Tag},
	fetch_vars(Rest, AST, NewCtx);
fetch_vars([#e_vardef{} = Hd | Rest], AST, {#e_vars{type_map = TypeMap} = Vars, Names, InitCode, false, Tag}) ->
	#e_vardef{name = Name, type = Type, loc = Loc, init_value = InitialValue} = Hd,
	check_name_conflict(Name, Vars, Loc),
	NewCtx = {Vars#e_vars{type_map = TypeMap#{Name => Type}}, [Name | Names], InitCode, false, Tag},
	fetch_vars(Rest, append_to_ast(AST, Name, InitialValue, Loc), NewCtx);
fetch_vars([#e_function_raw{name = Name, loc = Loc} = Hd | Rest], AST, {GlobalVars, _, _, _, _} = Ctx) ->
	#e_function_raw{ret_type = Ret, params = Params, stmts = Stmts, interrupt = Interrupt} = Hd,
	{ParamVars, [], ParamInitCode} = fetch_vars(Params, [], {#e_vars{}, [], [], true, local}),
	e_util:assert(ParamInitCode =:= [], {Loc, "function params can not have default value"}),
	check_variable_conflict(GlobalVars, ParamVars),
	{LocalVars, NewStmts, []} = fetch_vars(Stmts, [], {ParamVars, [], [], false, local}),
	check_variable_conflict(GlobalVars, LocalVars),
	FnType = #e_fn_type{params = get_values_by_defs(Params, ParamVars), ret = Ret, loc = Loc},
	ParamNames = e_util:names_of_var_defs(Params),
	check_label_conflict(NewStmts, #{}),
	Fn = #e_function{name = Name, vars = LocalVars, param_names = ParamNames, type = FnType, stmts = NewStmts, loc = Loc, interrupt = Interrupt},
	fetch_vars(Rest, [Fn | AST], Ctx);
fetch_vars([#e_struct_raw{name = Name, fields = RawFields, loc = Loc} | Rest], AST, Ctx) ->
	%% struct can have default value
	{Fields, [], StructInitCode} = fetch_vars(RawFields, [], {#e_vars{}, [], [], true, none}),
	FieldInitMap = struct_init_to_map(StructInitCode, #{}),
	S = #e_struct{name = Name, fields = Fields, default_value_map = FieldInitMap, loc = Loc},
	fetch_vars(Rest, [S | AST], Ctx);
fetch_vars([Any | Rest], AST, Ctx) ->
	fetch_vars(Rest, [Any | AST], Ctx);
fetch_vars([], AST, {#e_vars{names = OldNames} = Vars, Names, InitCode, _, Tag}) ->
	NewVars = Vars#e_vars{names = OldNames ++ lists:reverse(Names), tag = Tag},
	{NewVars, lists:reverse(AST), lists:reverse(InitCode)}.

-spec append_to_ast([e_stmt()], atom(), e_expr(), location()) -> e_ast().
append_to_ast(AST, VarName, InitialValue, Loc) when InitialValue =/= none ->
	[?OP2('=', #e_varref{name = VarName, loc = Loc}, InitialValue, Loc) | AST];
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
	OffsetMapNew = maps:map(fun(_, {O, S}) -> {O - Num, S} end, OffsetMap),
	Vars#e_vars{offset_map = OffsetMapNew, shifted_size = Size - Num}.

-spec shift_offset_middle(#e_vars{}) -> #e_vars{}.
shift_offset_middle(#e_vars{size = Size} = Vars) ->
	shift_offset(Vars, Size div 2).

