-module(e_variable).
-export([fetch_variables/1]).
-include("e_record_definition.hrl").

-spec fetch_variables(e_ast_raw()) -> {#e_vars{}, e_ast_raw(), e_ast_raw()}.
fetch_variables(AST) ->
	fetch_variables(prepare_struct_init_expr(AST), [], {#e_vars{}, [], [], true, global}).

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
fix_struct_init(#e_struct_init_raw_expr{name = Name, fields = Fields, line = Line}) ->
	#e_struct_init_expr{name = Name, field_value_map = struct_init_to_map(Fields, #{}), line = Line};
fix_struct_init(#e_array_init_expr{elements = Elements} = A) ->
	A#e_array_init_expr{elements = lists:map(fun fix_struct_init/1, Elements)};
fix_struct_init(#e_vardef{init_value = InitialValue} = V) ->
	V#e_vardef{init_value = fix_struct_init(InitialValue)};
fix_struct_init(#e_op{data = Operands} = O) ->
	O#e_op{data = lists:map(fun fix_struct_init/1, Operands)};
fix_struct_init(Any) ->
	Any.

-spec struct_init_to_map([e_expr()], #{atom() => e_expr()}) -> #{atom() => e_expr()}.
struct_init_to_map([#e_op{tag = '=', data = [#e_varref{name = Field}, Val]} | Rest], ExprMap) ->
	struct_init_to_map(Rest, ExprMap#{Field => fix_struct_init(Val)});
struct_init_to_map([], ExprMap) ->
	ExprMap.

%% In function expressions, the init code of defvar can not be simply fetched out from the code,
%% it should be replaced as assignment in the same place.

-type fetch_variables_state() ::
	{
	Vars ::#e_vars{},
	Names :: [atom()],
	AST :: e_ast(),
	CollectInitCode :: boolean(),
	Tag :: e_var_type()
	}.

%% Caution: fetch_variables/3 generate IN-COMPLETE `#e_vars{}` values here. (For functions, structs and global variables)
%% (Only `names`, `type_map` and `tag` fields are updated. `size`, `align` and `offset_map` fields are to be updated by functions in `e_size.erl`)

-spec fetch_variables(e_ast_raw(), e_ast_raw(), fetch_variables_state()) -> {#e_vars{}, e_ast(), e_ast()}.
fetch_variables([#e_vardef{} = Hd | Rest], AST, {#e_vars{type_map = TypeMap} = Vars, Names, InitCode, true, Tag}) ->
	#e_vardef{name = Name, type = Type, line = Line, init_value = InitialValue} = Hd,
	ensure_no_name_conflict(Name, Vars, Line),
	NewInitCode = append_to_ast(InitCode, Name, InitialValue, Line),
	NewVars = Vars#e_vars{type_map = TypeMap#{Name => Type}},
	NewCtx = {NewVars, [Name | Names], NewInitCode, true, Tag},
	fetch_variables(Rest, AST, NewCtx);
fetch_variables([#e_vardef{} = Hd | Rest], AST, {#e_vars{type_map = TypeMap} = Vars, Names, InitCode, false, Tag}) ->
	#e_vardef{name = Name, type = Type, line = Line, init_value = InitialValue} = Hd,
	ensure_no_name_conflict(Name, Vars, Line),
	NewCtx = {Vars#e_vars{type_map = TypeMap#{Name => Type}}, [Name | Names], InitCode, false, Tag},
	fetch_variables(Rest, append_to_ast(AST, Name, InitialValue, Line), NewCtx);
fetch_variables([#e_function_raw{} = Hd | Rest], AST, {GlobalVars, _, _, _, _} = Ctx) ->
	#e_function_raw{name = Name, ret_type = Ret, params = Params, stmts = Stmts, line = Line} = Hd,
	{ParamVars, [], ParamInitCode} = fetch_variables(Params, [], {#e_vars{}, [], [], true, local}),
	e_util:assert(ParamInitCode =:= [], {Line, "function params can not have default value"}),
	check_variable_conflict(GlobalVars, ParamVars),
	{LocalVars, NewStmts, []} = fetch_variables(Stmts, [], {ParamVars, [], [], false, local}),
	check_variable_conflict(GlobalVars, LocalVars),
	FnType = #e_fn_type{params = get_values_by_defs(Params, ParamVars), ret = Ret, line = Line},
	ParamNames = e_util:names_of_var_defs(Params),
	check_label_conflict(NewStmts, #{}),
	Fn = #e_function{name = Name, vars = LocalVars, param_names = ParamNames, type = FnType, stmts = NewStmts, line = Line},
	fetch_variables(Rest, [Fn | AST], Ctx);
fetch_variables([#e_struct_raw{name = Name, fields = RawFields, line = Line} | Rest], AST, Ctx) ->
	%% struct can have default value
	{Fields, [], StructInitCode} = fetch_variables(RawFields, [], {#e_vars{}, [], [], true, none}),
	FieldInitMap = struct_init_to_map(StructInitCode, #{}),
	S = #e_struct{name = Name, fields = Fields, default_value_map = FieldInitMap, line = Line},
	fetch_variables(Rest, [S | AST], Ctx);
fetch_variables([Any | Rest], AST, Ctx) ->
	fetch_variables(Rest, [Any | AST], Ctx);
fetch_variables([], AST, {#e_vars{names = OldNames} = Vars, Names, InitCode, _, Tag}) ->
	NewVars = Vars#e_vars{names = OldNames ++ lists:reverse(Names), tag = Tag},
	{NewVars, lists:reverse(AST), lists:reverse(InitCode)}.

-spec append_to_ast([e_stmt()], atom(), e_expr(), integer()) -> e_ast().
append_to_ast(AST, VarName, InitialValue, Line) when InitialValue =/= none ->
	[#e_op{tag = '=', data = [#e_varref{name = VarName, line = Line}, InitialValue], line = Line} | AST];
append_to_ast(AST, _, _, _) ->
	AST.

-spec check_variable_conflict(#e_vars{}, #e_vars{}) -> ok.
check_variable_conflict(#e_vars{type_map = GlobalVarMap}, #e_vars{type_map = LocalVarMap}) ->
	case maps:to_list(maps:with(maps:keys(GlobalVarMap), LocalVarMap)) of
		[{Name, T} | _] ->
			throw_name_conflict(Name, element(2, T));
		[] ->
			ok
	end.

-spec check_label_conflict([e_stmt()], #{atom() => integer()}) -> ok.
check_label_conflict([#e_label{name = Name, line = Line} | Rest], Map) ->
	case maps:find(Name, Map) of
		{ok, Line0} ->
			e_util:ethrow(Line, "label \"~s\" conflict with line ~w", [Name, Line0]);
		error ->
			check_label_conflict(Rest, Map#{Name => Line})
	end;
check_label_conflict([_ | Rest], Map) ->
	check_label_conflict(Rest, Map);
check_label_conflict([], _) ->
	ok.

-spec ensure_no_name_conflict(atom(), #e_vars{}, integer()) -> ok.
ensure_no_name_conflict(Name, #e_vars{type_map = VarMap}, Line) ->
	case maps:find(Name, VarMap) of
		{ok, _} ->
			throw_name_conflict(Name, Line);
		_ ->
			ok
	end.

-spec throw_name_conflict(atom(), integer()) -> no_return().
throw_name_conflict(Name, Line) ->
	throw({Line, e_util:fmt("name ~s has already been used", [Name])}).

-spec get_values_by_defs([#e_vardef{}], #e_vars{}) -> [any()].
get_values_by_defs(DefList, #e_vars{type_map = Map}) ->
	e_util:get_values_by_keys(e_util:names_of_var_defs(DefList), Map).

