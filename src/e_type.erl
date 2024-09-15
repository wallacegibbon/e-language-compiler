-module(e_type).
-export([check_types_in_ast/2, check_type_in_stmts/2, type_of_node/2, inc_pointer_depth/2, dec_pointer_depth/2]).
-export([replace_typeof_in_ast/2, replace_typeof_in_stmts/2, replace_typeof_in_vars/2]).
-export_type([interface_context/0, context/0]).
-include("e_record_definition.hrl").

-type interface_context() ::
	{
	GlobalVars :: #e_vars{},
	FnTypeMap :: #{atom() := #e_fn_type{}},
	StructMap :: #{atom() => #e_struct{}}
	}.

-spec check_types_in_ast(e_ast(), interface_context()) -> ok.
check_types_in_ast([#e_function{type = FnType} = Fn | Rest], {GlobalVars, FnTypeMap, StructMap} = Ctx) ->
	#e_function{vars = LocalVars, param_names = ParamNames, stmts = Stmts} = Fn,
	#e_vars{type_map = TypeMap} = LocalVars,
	Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
	Ctx1 = {Vars, FnTypeMap, StructMap, FnType#e_fn_type.ret},
	maps:foreach(fun(_, T) -> check_type(T, Ctx1) end, TypeMap),
	maps:foreach(fun(_, T) -> check_parameter_type(T, Ctx1) end, maps:with(ParamNames, TypeMap)),
	check_ret_type(FnType#e_fn_type.ret, Ctx1),
	%% The `#e_fn_type.ret` is used to check the operand of `return` statement.
	%% TODO: when user did not write `return` statement, checks are missed.
	check_type(FnType#e_fn_type.ret, Ctx1),
	type_of_nodes(Stmts, Ctx1),
	check_types_in_ast(Rest, Ctx);
check_types_in_ast([#e_struct{name = Name} = S | Rest], {GlobalVars, FnTypeMap, StructMap} = Ctx) ->
	#e_struct{fields = #e_vars{type_map = FieldTypeMap}, default_value_map = ValMap} = S,
	Ctx1 = {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}},
	maps:foreach(fun(_, T) -> check_type(T, Ctx1) end, FieldTypeMap),
	%% check the default values for fields
	check_types_in_struct_fields(FieldTypeMap, ValMap, Name, Ctx1),
	check_types_in_ast(Rest, Ctx);
check_types_in_ast([_ | Rest], Ctx) ->
	check_types_in_ast(Rest, Ctx);
check_types_in_ast([], _) ->
	ok.

-spec check_type_in_stmts([e_stmt()], interface_context()) -> ok.
check_type_in_stmts(Stmts, {GlobalVars, FnTypeMap, StructMap}) ->
	type_of_nodes(Stmts, {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}}),
	ok.

%% The `typeof` keyword will make the compiler complex without many benifits. So we drop it.
%% Implementing `typeof` is easy. But avoiding the recursive definition problem will make the code complex.
%% (e.g. `fn a(): typeof(a)`, `b: {typeof(b), 10}`, etc.)
-spec replace_typeof_in_ast(e_ast(), interface_context()) -> e_ast().
replace_typeof_in_ast([#e_function{} = Fn | Rest], {GlobalVars, FnTypeMap, StructMap} = Ctx) ->
	#e_function{vars = LocalVars, stmts = Stmts, type = FnType} = Fn,
	Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
	Ctx1 = {Vars, FnTypeMap, StructMap, FnType#e_fn_type.ret},
	Fn1 = Fn#e_function{vars = replace_typeof_in_vars(LocalVars, Ctx), type = replace_typeof_in_type(FnType, Ctx1), stmts = replace_typeof_in_stmts(Stmts, Ctx)},
	[Fn1 | replace_typeof_in_ast(Rest, Ctx)];
replace_typeof_in_ast([#e_struct{} = S | Rest], {GlobalVars, FnTypeMap, StructMap} = Ctx) ->
	#e_struct{fields = Fields, default_value_map = DefaultValueMap} = S,
	Ctx1 = {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}},
	S1 = S#e_struct{fields = replace_typeof_in_vars(Fields, Ctx), default_value_map = maps:map(fun(_, V) -> replace_typeof(V, Ctx1) end, DefaultValueMap)},
	[S1 | replace_typeof_in_ast(Rest, Ctx)];
replace_typeof_in_ast([Any | Rest], Ctx) ->
	[Any | replace_typeof_in_ast(Rest, Ctx)];
replace_typeof_in_ast([], _) ->
	[].

-spec replace_typeof_in_stmts([e_stmt()], interface_context()) -> [e_stmt()].
replace_typeof_in_stmts(Stmts, {GlobalVars, FnTypeMap, StructMap}) ->
	Ctx = {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}},
	e_util:expr_map(fun(E) -> replace_typeof(E, Ctx) end, Stmts).

-spec replace_typeof_in_vars(#e_vars{}, interface_context()) -> #e_vars{}.
replace_typeof_in_vars(#e_vars{type_map = TypeMap} = Vars, {GlobalVars, FnTypeMap, StructMap}) ->
	Ctx = {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}},
	Vars#e_vars{type_map = maps:map(fun(_, T) -> replace_typeof_in_type(T, Ctx) end, TypeMap)}.

-type context() ::
	{
	GlobalVars :: #e_vars{},
	FnTypeMap :: #{atom() := #e_fn_type{}},
	StructMap :: #{atom() => #e_struct{}},
	ReturnType :: e_type()
	}.

-spec replace_typeof(e_expr(), context()) -> e_expr().
replace_typeof(#e_type_convert{type = #e_typeof{expr = Expr}} = E, Ctx) ->
	E#e_type_convert{type = type_of_node(Expr, Ctx)};
replace_typeof(#e_op{tag = {call, Callee}, data = Data} = E, Ctx) ->
	E#e_op{tag = {call, replace_typeof(Callee, Ctx)}, data = lists:map(fun(V) -> replace_typeof(V, Ctx) end, Data)};
replace_typeof(#e_op{tag = {sizeof, Type}} = E, Ctx) ->
	E#e_op{tag = {sizeof, replace_typeof_in_type(Type, Ctx)}};
replace_typeof(#e_op{tag = {alignof, Type}} = E, Ctx) ->
	E#e_op{tag = {alignof, replace_typeof_in_type(Type, Ctx)}};
replace_typeof(#e_op{data = Data} = E, Ctx) ->
	E#e_op{data = lists:map(fun(V) -> replace_typeof(V, Ctx) end, Data)};
replace_typeof(#e_type_convert{expr = Expr} = C, Ctx) ->
	C#e_type_convert{expr = replace_typeof(Expr, Ctx)};
replace_typeof(Any, _) ->
	Any.

%% `typeof` can appear in another type like array `{typeof(a), 10}` or convert expressions like `a as typeof(b)`.
-spec replace_typeof_in_type(e_type(), context()) -> e_type().
replace_typeof_in_type(#e_array_type{elem_type = ElemType} = Type, Ctx) ->
	Type#e_array_type{elem_type = replace_typeof_in_type(ElemType, Ctx)};
replace_typeof_in_type(#e_fn_type{params = Params, ret = Ret} = Type, Ctx) ->
	Type#e_fn_type{params = lists:map(fun(T) -> replace_typeof_in_type(T, Ctx) end, Params), ret = replace_typeof_in_type(Ret, Ctx)};
replace_typeof_in_type(#e_basic_type{} = Type, _) ->
	Type;
replace_typeof_in_type(#e_typeof{expr = Expr}, Ctx) ->
	replace_typeof_in_type(type_of_node(Expr, Ctx), Ctx).

-spec type_of_nodes([e_stmt()], context()) -> [e_type()].
type_of_nodes(Stmts, Ctx) ->
	lists:map(fun(Expr) -> type_of_node(Expr, Ctx) end, Stmts).

-spec type_of_node(e_stmt(), context()) -> e_type().
type_of_node(#e_op{tag = '=', data = [#e_op{tag = '.', data = [Op11, Op12], loc = DotLoc}, Op2], loc = Loc}, {_, _, StructMap, _} = Ctx) ->
	Op1Type = type_of_struct_field(type_of_node(Op11, Ctx), Op12, StructMap, DotLoc),
	Op1TypeFixed = check_type(Op1Type, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	compare_expect_left(Op1TypeFixed, Op2Type, Loc);
type_of_node(#e_op{tag = '=', data = [#e_op{tag = '^', data = [SubOp, _]}, Op2], loc = Loc}, Ctx) ->
	Op1Type = dec_pointer_depth(type_of_node(SubOp, Ctx), Loc),
	Op2Type = type_of_node(Op2, Ctx),
	compare_expect_left(Op1Type, Op2Type, Loc);
type_of_node(#e_op{tag = '=', data = [#e_varref{} = Op1, Op2], loc = Loc}, Ctx) ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	compare_expect_left(Op1Type, Op2Type, Loc);
type_of_node(#e_op{tag = '=', data = [Any, _], loc = Loc}, _) ->
	e_util:ethrow(Loc, "invalid left value (~s)", [e_util:stmt_to_str(Any)]);
type_of_node(#e_op{tag = '.', data = [Op1, Op2], loc = Loc}, {_, _, StructMap, _} = Ctx) ->
	T1 = type_of_struct_field(type_of_node(Op1, Ctx), Op2, StructMap, Loc),
	check_type(T1, Ctx);
type_of_node(#e_op{tag = '+', data = [Op1, Op2], loc = Loc}, Ctx) ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	Checkers = [fun are_numbers_of_same_type/2, fun are_pointer_and_integer_ignore_order/2],
	case number_check_chain(Op1Type, Op2Type, Checkers) of
		{true, T} ->
			T;
		false ->
			e_util:ethrow(Loc, type_error_of('+', Op1Type, Op2Type))
	end;
%% `integer + pointer` is valid, but `integer - pointer` is invalid
type_of_node(#e_op{tag = '-', data = [Op1, Op2], loc = Loc}, Ctx) ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	case are_pointers_of_same_type(Op1Type, Op2Type) of
		true ->
			%% pointer - pointer --> integer.
			#e_basic_type{class = integer, tag = word, loc = Loc};
		false->
			Checkers = [fun are_numbers_of_same_type/2, fun are_pointer_and_integer/2],
			case number_check_chain(Op1Type, Op2Type, Checkers) of
				{true, T} ->
					T;
				false ->
					e_util:ethrow(Loc, type_error_of('-', Op1Type, Op2Type))
			end
	end;
type_of_node(#e_op{tag = '^', data = [Operand, _], loc = Loc}, Ctx) ->
	case type_of_node(Operand, Ctx) of
		#e_basic_type{} = T ->
			dec_pointer_depth(T, Loc);
		_ ->
			e_util:ethrow(Loc, "invalid \"^\" on operand ~s", [e_util:stmt_to_str(Operand)])
	end;
type_of_node(#e_op{tag = Tag, data = [Op1, Op2], loc = Loc}, Ctx) when Tag =:= '*'; Tag =:= '/' ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	case are_numbers_of_same_type(Op1Type, Op2Type) of
		{true, T} ->
			T;
		false ->
			e_util:ethrow(Loc, type_error_of(Tag, Op1Type, Op2Type))
	end;
type_of_node(#e_op{tag = {call, FunExpr}, data = Args, loc = Loc}, Ctx) ->
	ArgTypes = type_of_nodes(Args, Ctx),
	case type_of_node(FunExpr, Ctx) of
		#e_fn_type{params = FnParamTypes, ret = FnRetType} ->
			case compare_types(ArgTypes, FnParamTypes) of
				true ->
					FnRetType;
				false ->
					e_util:ethrow(Loc, arguments_error_info(FnParamTypes, ArgTypes))
			end;
		T ->
			e_util:ethrow(Loc, "invalid function type: ~s", [type_to_str(T)])
	end;
%% the left operators are: and, or, band, bor, bxor, bsl, bsr, >, <, ...
type_of_node(#e_op{tag = Tag, data = [Op1, Op2], loc = Loc}, Ctx) ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	case are_integers(Op1Type, Op2Type) of
		{true, Type} ->
			Type;
		false ->
			e_util:ethrow(Loc, type_error_of(Tag, Op1Type, Op2Type))
	end;
type_of_node(#e_op{tag = '@', data = [Operand], loc = Loc}, Ctx) ->
	inc_pointer_depth(type_of_node(Operand, Ctx), Loc);
type_of_node(#e_op{tag = {sizeof, _}, loc = Loc}, _) ->
	#e_basic_type{class = integer, tag = word, loc = Loc};
type_of_node(#e_op{tag = {alignof, _}, loc = Loc}, _) ->
	#e_basic_type{class = integer, tag = word, loc = Loc};
type_of_node(#e_op{data = [Operand]}, Ctx) ->
	type_of_node(Operand, Ctx);
type_of_node(#e_varref{name = Name, loc = Loc}, {#e_vars{type_map = TypeMap}, FnTypeMap, _, _} = Ctx) ->
	case e_util:map_find_multi(Name, [TypeMap, FnTypeMap]) of
		{ok, Type} ->
			%% The `loc` of the found type should be updated to the `loc` of the `e_varref`.
			check_type(setelement(2, Type, Loc), Ctx);
		notfound ->
			e_util:ethrow(Loc, "variable ~s is undefined", [Name])
	end;
type_of_node(#e_array_init_expr{elements = Elements, loc = Loc}, Ctx) ->
	ElementTypes = type_of_nodes(Elements, Ctx),
	case are_same_type(ElementTypes) of
		true ->
			#e_array_type{elem_type = hd(ElementTypes), length = length(ElementTypes), loc = Loc};
		false ->
			e_util:ethrow(Loc, "array init type conflict: {~s}", [join_types_to_str(ElementTypes)])
	end;
type_of_node(#e_struct_init_expr{} = S, {_, _, StructMap, _} = Ctx) ->
	#e_struct_init_expr{name = Name, field_value_map = ValMap, loc = Loc} = S,
	case maps:find(Name, StructMap) of
		{ok, #e_struct{fields = #e_vars{type_map = FieldTypeMap}}} ->
			check_types_in_struct_fields(FieldTypeMap, ValMap, Name, Ctx),
			#e_basic_type{class = struct, tag = Name, loc = Loc};
		_ ->
			e_util:ethrow(Loc, "type ~s is not found", [Name])
	end;
type_of_node(#e_type_convert{expr = Expr, type = Type, loc = Loc}, Ctx) ->
	convert_type(type_of_node(Expr, Ctx), Type, Loc);
type_of_node(#e_float{loc = Loc}, _) ->
	#e_basic_type{class = float, tag = float, loc = Loc};
type_of_node(#e_integer{loc = Loc}, _) ->
	#e_basic_type{class = integer, tag = word, loc = Loc};
type_of_node(#e_string{loc = Loc}, _) ->
	#e_basic_type{class = integer, p_depth = 1, tag = byte, loc = Loc};
type_of_node(#e_if_stmt{condi = Condi, then = Then, 'else' = Else, loc = Loc}, Ctx) ->
	type_of_node(Condi, Ctx),
	type_of_nodes(Then, Ctx),
	type_of_nodes(Else, Ctx),
	e_util:void_type(Loc);
type_of_node(#e_while_stmt{condi = Condi, stmts = Stmts, loc = Loc}, Ctx) ->
	type_of_node(Condi, Ctx),
	type_of_nodes(Stmts, Ctx),
	e_util:void_type(Loc);
type_of_node(#e_return_stmt{expr = Expr, loc = Loc}, {_, _, _, FnRetType} = Ctx) ->
	RealRet = type_of_node(Expr, Ctx),
	case compare_type(RealRet, FnRetType) of
		true ->
			RealRet;
		false ->
			e_util:ethrow(Loc, type_error_of('.vs.', FnRetType, RealRet))
	end;
type_of_node(#e_goto_stmt{loc = Loc}, _) ->
	e_util:void_type(Loc);
type_of_node(#e_label{loc = Loc}, _) ->
	e_util:void_type(Loc);
type_of_node(Any, _) ->
	e_util:ethrow(element(2, Any), "invalid statement: ~s~n", [e_util:stmt_to_str(Any)]).

%% Functions can be converted to any kind of pointers in current design.
-spec convert_type(e_type(), e_type(), location()) -> e_type().
convert_type(#e_fn_type{}, #e_basic_type{p_depth = D} = Type, _) when D > 0 ->
	Type;
convert_type(#e_basic_type{p_depth = D}, #e_fn_type{} = Type, _) when D > 0 ->
	Type;
convert_type(#e_basic_type{p_depth = D1}, #e_basic_type{p_depth = D2} = Type, _) when D1 > 0, D2 > 0 ->
	Type;
convert_type(#e_basic_type{class = integer, p_depth = 0}, #e_basic_type{p_depth = D2} = Type, _) when D2 > 0 ->
	Type;
convert_type(#e_basic_type{class = integer, p_depth = 0}, #e_basic_type{class = integer, p_depth = 0} = Type, _) ->
	Type;
convert_type(ExprType, Type, Loc) ->
	e_util:ethrow(Loc, type_error_of(to, ExprType, Type)).

-spec compare_expect_left(e_type(), e_type(), location()) -> e_type().
compare_expect_left(Type1, Type2, Loc) ->
	case compare_type(Type1, Type2) of
		true ->
			Type1;
		false ->
			e_util:ethrow(Loc, type_error_of('=', Type1, Type2))
	end.

-spec arguments_error_info([e_type()], [e_type()]) -> string().
arguments_error_info(FnParamTypes, ArgsTypes) ->
	e_util:fmt("args should be (~s), not (~s)", [join_types_to_str(FnParamTypes), join_types_to_str(ArgsTypes)]).

-spec inc_pointer_depth(e_type(), location()) -> e_type().
inc_pointer_depth(#e_basic_type{p_depth = N} = T, _) ->
	T#e_basic_type{p_depth = N + 1};
inc_pointer_depth(#e_array_type{elem_type = #e_basic_type{} = T}, Loc) ->
	inc_pointer_depth(T, Loc);
inc_pointer_depth(T, Loc) ->
	e_util:ethrow(Loc, "'@' on type ~s is invalid", [type_to_str(T)]).

-spec dec_pointer_depth(e_type(), location()) -> e_type().
dec_pointer_depth(#e_basic_type{p_depth = N} = T, _) when N > 0 ->
	T#e_basic_type{p_depth = N - 1};
dec_pointer_depth(T, Loc) ->
	e_util:ethrow(Loc, "'^' on type ~s is invalid", [type_to_str(T)]).

-spec check_types_in_struct_fields(#{atom() => e_type()}, #{atom() := e_expr()}, atom(), context()) -> ok.
check_types_in_struct_fields(FieldTypeMap, ValMap, StructName, Ctx) ->
	maps:foreach(fun(N, Val) -> check_struct_field(FieldTypeMap, N, Val, StructName, Ctx) end, ValMap).

-spec check_struct_field(#{atom() => e_type()}, atom(), e_expr(), atom(), context()) -> ok.
check_struct_field(FieldTypeMap, FieldName, Val, StructName, Ctx) ->
	Loc = element(2, Val),
	ExpectedType = get_field_type(FieldName, FieldTypeMap, StructName, Loc),
	%% there may be `typeof` inside `ExpectedType`, call `check_type` to resolve `typeof`.
	ExpectedTypeFixed = check_type(ExpectedType, Ctx),
	GivenType = type_of_node(Val, Ctx),
	case compare_type(ExpectedTypeFixed, GivenType) of
		true ->
			ok;
		false ->
			e_util:ethrow(Loc, type_error_of('=', ExpectedTypeFixed, GivenType))
	end.

-spec are_same_type([e_type()]) -> boolean().
are_same_type([#e_basic_type{class = integer, p_depth = 0}, #e_basic_type{class = integer, p_depth = 0} = T | Rest]) ->
	are_same_type([T | Rest]);
are_same_type([#e_basic_type{class = float, p_depth = 0}, #e_basic_type{class = float, p_depth = 0} = T | Rest]) ->
	are_same_type([T | Rest]);
are_same_type([T1, T2 | Rest]) ->
	case are_same_type_ignore_pos(T1, T2) of
		true ->
			are_same_type([T2 | Rest]);
		false ->
			false
	end;
are_same_type([_]) ->
	true;
are_same_type(_) ->
	false.

-spec are_same_type_ignore_pos(e_type(), e_type()) -> boolean().
are_same_type_ignore_pos(T1, T2) ->
	setelement(2, T1, {0, 0}) =:= setelement(2, T2, {0, 0}).

-spec type_of_struct_field(e_type(), #e_varref{}, #{atom() => #e_struct{}}, location()) -> e_type().
type_of_struct_field(#e_basic_type{class = struct, p_depth = 0} = S, #e_varref{name = FieldName}, StructMap, Loc) ->
	#e_basic_type{tag = Name} = S,
	#e_struct{fields = #e_vars{type_map = FieldTypeMap}} = e_util:get_struct_from_type(S, StructMap),
	get_field_type(FieldName, FieldTypeMap, Name, Loc);
type_of_struct_field(T, _, _, Loc) ->
	e_util:ethrow(Loc, "the left operand for \".\" is the wrong type: ~s", [type_to_str(T)]).

-spec get_field_type(atom(), #{atom() => e_type()}, atom(), location()) -> e_type().
get_field_type(FieldName, FieldTypeMap, StructName, Loc) ->
	case maps:find(FieldName, FieldTypeMap) of
		{ok, Type} ->
			Type;
		error ->
			e_util:ethrow(Loc, "~s.~s does not exist", [StructName, FieldName])
	end.

-spec compare_types([e_type()], [e_type()]) -> boolean().
compare_types([T1 | Types1], [T2 | Types2]) ->
	case compare_type(T1, T2) of
		true ->
			compare_types(Types1, Types2);
		false ->
			false
	end;
compare_types([], []) ->
	true;
compare_types(_, _) ->
	false.

-spec compare_type(e_type(), e_type()) -> boolean().
compare_type(#e_fn_type{params = P1, ret = R1}, #e_fn_type{params = P2, ret = R2}) ->
	compare_types(P1, P2) and compare_type(R1, R2);
compare_type(#e_array_type{elem_type = E1, length = L1}, #e_array_type{elem_type = E2, length = L2}) ->
	compare_type(E1, E2) and (L1 =:= L2);
compare_type(#e_basic_type{class = integer, p_depth = 0}, #e_basic_type{class = integer, p_depth = 0}) ->
	true;
compare_type(#e_basic_type{class = C, tag = T, p_depth = P}, #e_basic_type{class = C, tag = T, p_depth = P}) ->
	true;
compare_type(_, _) ->
	false.

-type number_check_result() :: {true, e_type()} | false.
-type number_check_fn() :: fun((e_type(), e_type()) -> number_check_result()).

-spec number_check_chain(e_type(), e_type(), [number_check_fn()]) -> number_check_result().
number_check_chain(T1, T2, [Fn | RestFns]) ->
	case Fn(T1, T2) of
		{true, _} = R ->
			R;
		false ->
			number_check_chain(T1, T2, RestFns)
	end;
number_check_chain(_, _, []) ->
	false.

-spec are_pointer_and_integer_ignore_order(e_type(), e_type()) -> number_check_result().
are_pointer_and_integer_ignore_order(T1, T2) ->
	case are_pointer_and_integer(T1, T2) of
		{true, _} = R ->
			R;
		false ->
			are_pointer_and_integer(T2, T1)
	end.

-spec are_pointer_and_integer(e_type(), e_type()) -> number_check_result().
are_pointer_and_integer(#e_basic_type{p_depth = N} = Type, #e_basic_type{class = integer, p_depth = 0}) when N > 0 ->
	{true, Type};
are_pointer_and_integer(#e_fn_type{} = Type, #e_basic_type{class = integer, p_depth = 0}) ->
	{true, Type};
are_pointer_and_integer(_, _) ->
	false.

-spec are_pointers_of_same_type(e_type(), e_type()) -> boolean().
are_pointers_of_same_type(#e_basic_type{class = C, tag = T, p_depth = N}, #e_basic_type{class = C, tag = T, p_depth = N}) ->
	true;
are_pointers_of_same_type(#e_fn_type{}, #e_fn_type{}) ->
	true;
are_pointers_of_same_type(_, _) ->
	false.

-spec are_numbers_of_same_type(e_type(), e_type()) -> number_check_result().
are_numbers_of_same_type(T1, T2) ->
	number_check_chain(T1, T2, [fun are_integers/2, fun are_floats/2]).

-spec are_integers(e_type(), e_type()) -> number_check_result().
are_integers(#e_basic_type{class = integer, p_depth = 0} = T1, #e_basic_type{class = integer, p_depth = 0} = T2) ->
	{true, bigger_type(T1, T2)};
are_integers(_, _) ->
	false.

-spec are_floats(e_type(), e_type()) -> number_check_result().
are_floats(#e_basic_type{class = float, p_depth = 0} = T1, #e_basic_type{class = float, p_depth = 0} = T2) ->
	{true, bigger_type(T1, T2)};
are_floats(_, _) ->
	false.

-spec bigger_type(e_type(), e_type()) -> e_type().
bigger_type(#e_basic_type{class = integer, tag = word} = T1, #e_basic_type{class = integer}) ->
	T1;
bigger_type(#e_basic_type{class = integer}, #e_basic_type{class = integer} = T2) ->
	T2;
bigger_type(#e_basic_type{class = float} = T, #e_basic_type{class = float}) ->
	T.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

-define(IOBJ(Tag, PDepth), #e_basic_type{class = integer, tag = Tag, p_depth = PDepth}).
-define(FOBJ(PDepth), #e_basic_type{class = float, tag = float, p_depth = PDepth}).

bigger_type_test() ->
	?assertMatch(?IOBJ(word, 0), bigger_type(?IOBJ(byte, 0), ?IOBJ(word, 0))),
	ok.

are_numbers_of_same_type_test() ->
	?assertMatch({true, ?IOBJ(word, 0)}, are_numbers_of_same_type(?IOBJ(word, 0), ?IOBJ(byte, 0))),
	?assertMatch(false, are_numbers_of_same_type(?IOBJ(i16, 0), ?FOBJ(0))),
	ok.

number_check_chain_test() ->
	L1 = [fun are_numbers_of_same_type/2, fun are_pointers_of_same_type/2, fun are_pointer_and_integer/2],
	?assertMatch({true, ?IOBJ(word, 0)}, number_check_chain(?IOBJ(word, 0), ?IOBJ(byte, 0), L1)),
	L2 = [fun are_numbers_of_same_type/2, fun are_pointers_of_same_type/2, fun are_pointer_and_integer/2],
	?assertMatch(false, number_check_chain(?IOBJ(word, 0), ?IOBJ(byte, 1), L2)),
	L3 = [fun are_numbers_of_same_type/2, fun are_pointers_of_same_type/2, fun are_pointer_and_integer_ignore_order/2],
	?assertMatch({true, ?IOBJ(byte, 1)}, number_check_chain(?IOBJ(word, 0), ?IOBJ(byte, 1), L3)),
	ok.

-endif.

-spec type_error_of(atom(), e_type(), e_type()) -> string().
type_error_of(Tag, TypeofOp1, TypeofOp2) ->
	e_util:fmt("type error: <~s> ~s <~s>", [type_to_str(TypeofOp1), Tag, type_to_str(TypeofOp2)]).

%% check type, ensure that all struct used by type exists.
-spec check_type(e_type(), context()) -> e_type().
check_type(#e_basic_type{class = struct} = Type, {_, _, StructMap, _}) ->
	#e_struct{} = e_util:get_struct_from_type(Type, StructMap),
	Type;
check_type(#e_basic_type{} = Type, _) ->
	Type;
check_type(#e_array_type{elem_type = #e_array_type{loc = Loc}}, _) ->
	e_util:ethrow(Loc, "nested array is not supported");
check_type(#e_array_type{elem_type = ElemType} = Type, Ctx) ->
	check_type(ElemType, Ctx),
	Type;
check_type(#e_fn_type{params = Params, ret = RetType} = Type, Ctx) ->
	lists:foreach(fun(T) -> check_type(T, Ctx) end, Params),
	check_type(RetType, Ctx),
	Type;
check_type(#e_typeof{expr = Expr}, Ctx) ->
	check_type(type_of_node(Expr, Ctx), Ctx).

-spec check_parameter_type(e_type(), context()) -> boolean.
check_parameter_type(#e_basic_type{p_depth = N}, _) when N > 0 ->
	true;
check_parameter_type(#e_basic_type{class = C}, _) when C =:= integer; C =:= float ->
	true;
check_parameter_type(T, _) ->
	e_util:ethrow(element(2, T), "invalid parameter type here").

check_ret_type(#e_basic_type{p_depth = N}, _) when N > 0 ->
	true;
check_ret_type(#e_basic_type{class = C}, _) when C =:= integer; C =:= float; C =:= void ->
	true;
check_ret_type(T, _) ->
	e_util:ethrow(element(2, T), "invalid returning type here").

-spec join_types_to_str([e_type()]) -> string().
join_types_to_str(Types) ->
	lists:join(",", lists:map(fun type_to_str/1, Types)).

-spec type_to_str(e_type()) -> string().
type_to_str(#e_typeof{expr = Expr}) ->
	e_util:fmt("typeof(~s)", [e_util:stmt_to_str(Expr)]);
type_to_str(#e_fn_type{params = Params, ret = RetType}) ->
	e_util:fmt("fun (~s): ~s", [join_types_to_str(Params), type_to_str(RetType)]);
type_to_str(#e_array_type{elem_type = Type, length = N}) ->
	e_util:fmt("{~s, ~w}", [type_to_str(Type), N]);
type_to_str(#e_struct_init_expr{name = Name}) ->
	atom_to_list(Name);
type_to_str(#e_basic_type{tag = Tag, p_depth = N}) when N > 0 ->
	e_util:fmt("(~s~s)", [Tag, lists:duplicate(N, "^")]);
type_to_str(#e_basic_type{tag = Tag, p_depth = 0}) ->
	atom_to_list(Tag).

