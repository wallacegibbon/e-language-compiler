-module(e_variable).
-export([fetch_variables/1]).
-include("e_record_definition.hrl").

-spec fetch_variables(e_ast_raw()) -> {e_ast_raw(), #{atom() => e_type()}, e_ast_raw()}.
fetch_variables(AST) ->
	{AST3, VarTypes, InitCode} = fetch_variables(prepare_struct_init_expr(AST), [], {#{}, [], true}),
	{AST3, VarTypes, InitCode}.

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
	A#e_array_init_expr{elements = fix_struct_init_expr_in_stmts(Elements)};
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
-spec fetch_variables(e_ast_raw(), e_ast_raw(), {#{atom() => e_type()}, e_ast(), CollectInitCode :: boolean()})
	-> {e_ast_raw(), #{atom() => e_type()}, e_ast_raw()}.

fetch_variables([#e_vardef{} = Hd | Rest], NewAST, {VarTypes, InitCode, true}) ->
	#e_vardef{name = Name, type = Type, line = Line, init_value = InitialValue} = Hd,
	ensure_no_name_conflict(Name, VarTypes, Line),
	NewCtx = {VarTypes#{Name => Type}, append_to_ast(InitCode, Name, InitialValue, Line), true},
	fetch_variables(Rest, NewAST, NewCtx);
fetch_variables([#e_vardef{} = Hd | Rest], NewAST, {VarTypes, InitCode, false}) ->
	#e_vardef{name = Name, type = Type, line = Line, init_value = InitialValue} = Hd,
	ensure_no_name_conflict(Name, VarTypes, Line),
	NewCtx = {VarTypes#{Name => Type}, InitCode, false},
	fetch_variables(Rest, append_to_ast(NewAST, Name, InitialValue, Line), NewCtx);
fetch_variables([#e_function_raw{} = Hd | Rest], NewAST, {GlobalVars, _, _} = Ctx) ->
	#e_function_raw{name = Name, ret_type = Ret, params = Params, stmts = Stmts, line = Line} = Hd,
	{[], ParamVars, ParamInitCode} = fetch_variables(Params, [], {#{}, [], true}),
	e_util:assert(ParamInitCode =:= [], {Line, "function params can not have default value"}),
	{NewStmts, FnVarTypes, []} = fetch_variables(Stmts, [], {ParamVars, [], false}),
	%% local variables should have different names from global variables.
	check_variable_conflict(GlobalVars, FnVarTypes),
	%% label names should be different from variables since the operand of goto could be a pointer.
	Labels = lists:filter(fun(E) -> element(1, E) =:= e_goto_label end, Stmts),
	check_label_conflict(Labels, GlobalVars, FnVarTypes),
	FnType = #e_fn_type{params = get_values_by_defs(Params, ParamVars), ret = Ret, line = Line},
	Vars = #e_vars{type_map = FnVarTypes},
	ParamNames = e_util:names_of_var_defs(Params),
	Fn = #e_function{name = Name, vars = Vars, stmts = NewStmts, param_names = ParamNames, type = FnType, line = Line},
	fetch_variables(Rest, [Fn | NewAST], Ctx);
fetch_variables([#e_struct_raw{name = Name, fields = RawFields, line = Line} | Rest], NewAST, Ctx) ->
	%% struct can have default value
	{[], FieldTypes, StructInitCode} = fetch_variables(RawFields, [], {#{}, [], true}),
	FieldInitMap = struct_init_to_map(StructInitCode, #{}),
	FieldNames = lists:map(fun(#e_vardef{name = N}) -> N end, RawFields),
	Fields = #e_vars{names = FieldNames, type_map = FieldTypes},
	S = #e_struct{name = Name, fields = Fields, default_value_map = FieldInitMap, line = Line},
	fetch_variables(Rest, [S | NewAST], Ctx);
fetch_variables([Any | Rest], NewAST, Ctx) ->
	fetch_variables(Rest, [Any | NewAST], Ctx);
fetch_variables([], NewAST, {VarTypes, InitCode, _}) ->
	{lists:reverse(NewAST), VarTypes, lists:reverse(InitCode)}.

-spec append_to_ast([e_stmt()], atom(), e_expr(), integer()) -> e_ast().
append_to_ast(AST, VarName, InitialValue, Line) when InitialValue =/= none ->
	[#e_op{tag = '=', data = [#e_varref{name = VarName, line = Line}, InitialValue], line = Line} | AST];
append_to_ast(AST, _, _, _) ->
	AST.

%% TODO: dialyzer went crazy here:
%% The pattern <[{'e_goto_label', Line, Name} | Rest], GlobalVars, LocalVars>
%% can never match the type <[],#{atom()=>_},#{atom()=>_}>
-spec check_label_conflict([#e_goto_label{}], #{atom() => e_type()}, #{atom() => e_type()}) -> ok.
check_label_conflict([#e_goto_label{name = Name, line = Line} | Rest], GlobalVars, LocalVars) ->
	ensure_no_name_conflict(Name, LocalVars, Line),
	ensure_no_name_conflict(Name, GlobalVars, Line),
	check_label_conflict(Rest, GlobalVars, LocalVars);
check_label_conflict([], _, _) ->
	ok.

-spec check_variable_conflict(#{atom() => e_type()}, #{atom() => e_type()}) -> ok.
check_variable_conflict(GlobalVars, LocalVars) ->
	case maps:to_list(maps:with(maps:keys(GlobalVars), LocalVars)) of
		[{Name, T} | _] ->
			throw_name_conflict(Name, element(2, T));
		[] ->
			ok
	end.

-spec ensure_no_name_conflict(atom(), #{atom() => e_type()}, integer()) -> ok.
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

-spec get_values_by_defs([#e_vardef{}], #{atom() => any()}) -> [any()].
get_values_by_defs(DefList, Map) ->
	e_util:get_values_by_keys(e_util:names_of_var_defs(DefList), Map).

