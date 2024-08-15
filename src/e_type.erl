-module(e_type).
-export([check_types_in_ast/3, check_type_in_stmts/3, type_of_node/2]).
-include("e_record_definition.hrl").

-type interface_context() :: {#{atom() => #e_fn_type{}}, #{atom() => #e_struct{}}}.

-spec check_types_in_ast(e_ast(), #e_vars{}, interface_context()) -> ok.
check_types_in_ast([#e_function{stmts = Stmts} = Fn | Rest], GlobalVars, {FnTypeMap, StructMap} = Maps) ->
	#e_function{vars = #e_vars{type_map = TypeMap} = LocalVars, type = FnType} = Fn,
	check_types(maps:values(TypeMap), StructMap),
	check_type(FnType#e_fn_type.ret, StructMap),
	Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
	%% The `#e_fn_type.ret` is used to check the operand of `return` statement.
	type_of_nodes(Stmts, {Vars, FnTypeMap, StructMap, FnType#e_fn_type.ret}),
	check_types_in_ast(Rest, GlobalVars, Maps);
check_types_in_ast([#e_struct{name = Name} = S | Rest], GlobalVars, {FnTypeMap, StructMap} = Maps) ->
	#e_struct{fields = #e_vars{type_map = FieldTypeMap}, default_value_map = ValMap} = S,
	check_types(maps:values(FieldTypeMap), StructMap),
	%% check the default values for fields
	Ctx = {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}},
	check_types_in_struct_fields(FieldTypeMap, ValMap, Name, Ctx),
	check_types_in_ast(Rest, GlobalVars, Maps);
check_types_in_ast([_ | Rest], GlobalVars, Maps) ->
	check_types_in_ast(Rest, GlobalVars, Maps);
check_types_in_ast([], _, _) ->
	ok.

-spec check_type_in_stmts([e_stmt()], #e_vars{}, interface_context()) -> ok.
check_type_in_stmts(Stmts, GlobalVars, {FnTypeMap, StructMap}) ->
	type_of_nodes(Stmts, {GlobalVars, FnTypeMap, StructMap, #e_basic_type{}}),
	ok.

-type context() ::
	{
	GlobalVars :: #e_vars{},
	FnTypeMap :: #{atom() := #e_fn_type{}},
	StructMap :: #{atom() => #e_struct{}},
	ReturnType :: e_type()
	}.

-spec type_of_nodes([e_stmt()], context()) -> [e_type()].
type_of_nodes(Stmts, Ctx) ->
	lists:map(fun(Expr) -> type_of_node(Expr, Ctx) end, Stmts).

-spec type_of_node(e_stmt(), context()) -> e_type().
type_of_node(#e_op{tag = '=', data = [#e_op{tag = '.', data = [Op11, Op12], loc = DotLoc}, Op2], loc = Loc}, {_, _, StructMap, _} = Ctx) ->
	Op1Type = type_of_struct_field(type_of_node(Op11, Ctx), Op12, StructMap, DotLoc),
	Op2Type = type_of_node(Op2, Ctx),
	compare_expect_left(Op1Type, Op2Type, Loc);
type_of_node(#e_op{tag = '=', data = [#e_op{tag = '^', data = [SubOp]}, Op2], loc = Loc}, Ctx) ->
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
	type_of_struct_field(type_of_node(Op1, Ctx), Op2, StructMap, Loc);
type_of_node(#e_op{tag = '+', data = [Op1, Op2], loc = Loc}, Ctx) ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	case are_both_number_of_same_type(Op1Type, Op2Type) of
		{true, T} ->
			T;
		false ->
			case is_pointer_and_integer(Op1Type, Op2Type) of
				{true, PointerType} ->
					PointerType;
				false ->
					e_util:ethrow(Loc, type_error_of_op2('+', Op1Type, Op2Type))
			end
	end;
%% integer + pointer is valid, but integer - pointer is invalid
type_of_node(#e_op{tag = '-', data = [Op1, Op2], loc = Loc}, Ctx) ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	case are_both_number_of_same_type(Op1Type, Op2Type) of
		{true, T} ->
			T;
		false ->
			case is_pointer_and_integer_ordered(Op1Type, Op2Type) of
				{true, PointerType} ->
					PointerType;
				false ->
					e_util:ethrow(Loc, type_error_of_op2('-', Op1Type, Op2Type))
			end
	end;
type_of_node(#e_op{tag = Tag, data = [Op1, Op2], loc = Loc}, Ctx) when Tag =:= '*'; Tag =:= '/' ->
	Op1Type = type_of_node(Op1, Ctx),
	Op2Type = type_of_node(Op2, Ctx),
	case are_both_number_of_same_type(Op1Type, Op2Type) of
		{true, T} ->
			T;
		false ->
			e_util:ethrow(Loc, type_error_of_op2(Tag, Op1Type, Op2Type))
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
	case are_both_integers(Op1Type, Op2Type) of
		true ->
			Op1Type;
		false ->
			e_util:ethrow(Loc, type_error_of_op2(Tag, Op1Type, Op2Type))
	end;
type_of_node(#e_op{tag = '^', data = [Operand], loc = Loc}, Ctx) ->
	case type_of_node(Operand, Ctx) of
		#e_basic_type{} = T ->
			dec_pointer_depth(T, Loc);
		_ ->
			e_util:ethrow(Loc, "invalid \"^\" on operand ~s", [e_util:stmt_to_str(Operand)])
	end;
type_of_node(#e_op{tag = '@', data = [Operand], loc = Loc}, Ctx) ->
	inc_pointer_depth(type_of_node(Operand, Ctx), Loc);
type_of_node(#e_op{tag = {sizeof, _}, loc = Loc}, _) ->
	#e_basic_type{class = integer, tag = usize, loc = Loc};
type_of_node(#e_op{tag = {alignof, _}, loc = Loc}, _) ->
	#e_basic_type{class = integer, tag = usize, loc = Loc};
type_of_node(#e_op{data = [Operand]}, Ctx) ->
	type_of_node(Operand, Ctx);
type_of_node(#e_varref{name = Name, loc = Loc}, {#e_vars{type_map = TypeMap}, FnTypeMap, StructMap, _}) ->
	case e_util:map_find_multi(Name, [TypeMap, FnTypeMap]) of
		{ok, Type} ->
			check_type(Type, StructMap),
			Type;
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
	#e_basic_type{class = float, tag = f64, loc = Loc};
type_of_node(#e_integer{loc = Loc}, _) ->
	#e_basic_type{class = integer, tag = i64, loc = Loc};
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
			e_util:ethrow(Loc, "ret type should be (~s), not (~s)", [type_to_str(FnRetType), type_to_str(RealRet)])
	end;
type_of_node(#e_goto_stmt{loc = Loc}, _) ->
	e_util:void_type(Loc);
type_of_node(#e_label{loc = Loc}, _) ->
	e_util:void_type(Loc).

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
	e_util:ethrow(Loc, "incompatible type: <~s> .vs. <~s>", [type_to_str(ExprType), type_to_str(Type)]).

-spec compare_expect_left(e_type(), e_type(), location()) -> e_type().
compare_expect_left(Type1, Type2, Loc) ->
	case compare_type(Type1, Type2) of
		true ->
			Type1;
		false ->
			e_util:ethrow(Loc, "type mismatch in \"~s = ~s\"", [type_to_str(Type1), type_to_str(Type2)])
	end.

-spec arguments_error_info([e_type()], [e_type()]) -> string().
arguments_error_info(FnParamTypes, ArgsTypes) ->
	e_util:fmt("args should be (~s), not (~s)", [join_types_to_str(FnParamTypes), join_types_to_str(ArgsTypes)]).

-spec inc_pointer_depth(e_type(), location()) -> e_type().
inc_pointer_depth(#e_basic_type{p_depth = PDepth} = T, _) ->
	T#e_basic_type{p_depth = PDepth + 1};
inc_pointer_depth(#e_array_type{elem_type = #e_basic_type{} = T}, Loc) ->
	inc_pointer_depth(T, Loc);
inc_pointer_depth(T, Loc) ->
	e_util:ethrow(Loc, "'@' on type ~s is invalid", [type_to_str(T)]).

-spec dec_pointer_depth(e_type(), location()) -> e_type().
dec_pointer_depth(#e_basic_type{p_depth = PDepth} = T, _) when PDepth > 0 ->
	T#e_basic_type{p_depth = PDepth - 1};
dec_pointer_depth(T, Loc) ->
	e_util:ethrow(Loc, "'^' on type ~s is invalid", [type_to_str(T)]).

-spec check_types_in_struct_fields(#{atom() => e_type()}, #{atom() := e_expr()}, atom(), context()) -> ok.
check_types_in_struct_fields(FieldTypeMap, ValMap, StructName, Ctx) ->
	maps:foreach(fun(N, Val) -> check_struct_field(FieldTypeMap, N, Val, StructName, Ctx) end, ValMap).

-spec check_struct_field(#{atom() => e_type()}, atom(), e_expr(), atom(), context()) -> ok.
check_struct_field(FieldTypeMap, FieldName, Val, StructName, {_, _, StructMap, _} = Ctx) ->
	Loc = element(2, Val),
	ExpectedType = get_field_type(FieldName, FieldTypeMap, StructName, Loc),
	check_type(ExpectedType, StructMap),
	GivenType = type_of_node(Val, Ctx),
	case compare_type(ExpectedType, GivenType) of
		true ->
			ok;
		false ->
			e_util:ethrow(Loc, "~s.~s type error: ~s = ~s", [StructName, FieldName, type_to_str(ExpectedType), type_to_str(GivenType)])
	end.

-spec are_same_type([e_type()]) -> boolean().
are_same_type([Type, Type | Rest]) ->
	are_same_type([Type | Rest]);
are_same_type([_]) ->
	true;
are_same_type(_) ->
	false.

-spec type_of_struct_field(e_type(), #e_varref{}, #{atom() => #e_struct{}}, location()) -> e_type().
type_of_struct_field(#e_basic_type{class = struct, tag = Name, p_depth = 0} = S, #e_varref{name = FieldName}, StructMap, Loc) ->
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

-spec is_pointer_and_integer_ordered(e_type(), e_type()) -> {true, e_type()} | false.
is_pointer_and_integer_ordered(#e_basic_type{p_depth = PDepth} = Type, #e_basic_type{class = integer, p_depth = 0}) when PDepth > 0 ->
	{true, Type};
is_pointer_and_integer_ordered(_, _) ->
	false.

-spec is_pointer_and_integer(e_type(), e_type()) -> {true, e_type()} | false.
is_pointer_and_integer(#e_basic_type{p_depth = PDepth} = Type, #e_basic_type{class = integer, p_depth = 0}) when PDepth > 0 ->
	{true, Type};
is_pointer_and_integer(#e_basic_type{class = integer, p_depth = 0}, #e_basic_type{p_depth = PDepth} = Type) when PDepth > 0 ->
	{true, Type};
is_pointer_and_integer(_, _) ->
	false.

-spec are_both_number_of_same_type(e_type(), e_type()) -> {true, e_type()} | false.
are_both_number_of_same_type(T1, T2) ->
	case are_both_integers(T1, T2) or are_both_floats(T1, T2) of
		true ->
			{true, T1};
		false ->
			false
	end.

-spec are_both_integers(e_type(), e_type()) -> boolean().
are_both_integers(#e_basic_type{p_depth = 0, class = integer}, #e_basic_type{p_depth = 0, class = integer}) ->
	true;
are_both_integers(_, _) ->
	false.

-spec are_both_floats(e_type(), e_type()) -> boolean().
are_both_floats(#e_basic_type{p_depth = 0, class = float}, #e_basic_type{p_depth = 0, class = float}) ->
	true;
are_both_floats(_, _) ->
	false.

-spec type_error_of_op2(atom(), e_type(), e_type()) -> string().
type_error_of_op2(Operator, TypeofOp1, TypeofOp2) ->
	e_util:fmt("type error in \"~s ~s ~s\"", [type_to_str(TypeofOp1), Operator, type_to_str(TypeofOp2)]).

-spec check_types([e_type()], #{atom() => #e_struct{}}) -> ok.
check_types(TypeList, StructMap) ->
	lists:foreach(fun(T) -> check_type(T, StructMap) end, TypeList).

%% check type, ensure that all struct used by type exists.
-spec check_type(e_type(), #{atom() => #e_struct{}}) -> any().
check_type(#e_basic_type{class = struct} = S, StructMap) ->
	e_util:get_struct_from_type(S, StructMap);
check_type(#e_basic_type{}, _) ->
	ok;
check_type(#e_array_type{elem_type = Type}, StructMap) ->
	case Type of
		#e_array_type{loc = Loc} ->
			e_util:ethrow(Loc, "nested array is not supported");
		_ ->
			check_type(Type, StructMap)
	end;
check_type(#e_fn_type{params = Params, ret = RetType}, StructMap) ->
	check_types(Params, StructMap),
	check_type(RetType, StructMap).

-spec join_types_to_str([e_type()]) -> string().
join_types_to_str(Types) ->
	lists:join(",", lists:map(fun type_to_str/1, Types)).

-spec type_to_str(e_type()) -> string().
type_to_str(#e_fn_type{params = Params, ret = RetType}) ->
	e_util:fmt("fun (~s): ~s", [join_types_to_str(Params), type_to_str(RetType)]);
type_to_str(#e_array_type{elem_type = Type, length = N}) ->
	e_util:fmt("{~s, ~w}", [type_to_str(Type), N]);
type_to_str(#e_basic_type{tag = Tag, p_depth = PDepth}) when PDepth > 0 ->
	e_util:fmt("(~s~s)", [Tag, lists:duplicate(PDepth, "^")]);
type_to_str(#e_basic_type{tag = Tag, p_depth = 0}) ->
	atom_to_list(Tag).

