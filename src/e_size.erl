-module(e_size).
-export([expand_sizeof_in_ast/2, expand_sizeof_in_stmts/2, fill_offsets/2, fill_var_offsets/2]).
-include("e_record_definition.hrl").

-type context() :: {StructMap :: #{atom() => #e_struct{}}, PointerWidth :: non_neg_integer()}.

-spec expand_sizeof_in_ast(e_ast(), context()) -> e_ast().
expand_sizeof_in_ast([#e_function{stmts = Stmts} = Fn | Rest], Ctx) ->
	[Fn#e_function{stmts = expand_sizeof_in_stmts(Stmts, Ctx)} | expand_sizeof_in_ast(Rest, Ctx)];
expand_sizeof_in_ast([#e_struct{default_value_map = FieldDefaults} = S | Rest], Ctx) ->
	[S#e_struct{default_value_map = expand_sizeof_in_map(FieldDefaults, Ctx)} | expand_sizeof_in_ast(Rest, Ctx)];
expand_sizeof_in_ast([], _) ->
	[].

expand_sizeof_in_map(Map, Ctx) ->
	maps:map(fun(_, V1) -> expand_sizeof_in_expr(V1, Ctx) end, Map).

-spec expand_sizeof_in_stmts([e_stmt()], context()) -> [e_stmt()].
expand_sizeof_in_stmts(Stmts, Ctx) ->
	e_util:expr_map(fun(E) -> expand_sizeof_in_expr(E, Ctx) end, Stmts).

-spec expand_sizeof_in_expr(e_expr(), context()) -> e_expr().
expand_sizeof_in_expr(#e_op{tag = {sizeof, T}, line = Line}, Ctx) ->
	try
		{e_integer, Line, size_of(T, Ctx)}
	catch
		I ->
			throw({Line, I})
	end;
expand_sizeof_in_expr(#e_op{data = Data} = E, Ctx) ->
	E#e_op{data = lists:map(fun(O) -> expand_sizeof_in_expr(O, Ctx) end, Data)};
expand_sizeof_in_expr(#e_struct_init_expr{field_value_map = ExprMap} = S, Ctx) ->
	S#e_struct_init_expr{field_value_map = expand_sizeof_in_map(ExprMap, Ctx)};
expand_sizeof_in_expr(#e_array_init_expr{elements = Elements} = A, Ctx) ->
	A#e_array_init_expr{elements = expand_sizeof_in_stmts(Elements, Ctx)};
expand_sizeof_in_expr(Any, _) ->
	Any.

-spec fill_offsets(e_ast(), context()) -> e_ast().
fill_offsets(AST, Ctx) ->
	lists:map(fun(E) -> fill_offsets_stmt(E, Ctx) end, AST).

-spec fill_offsets_stmt(e_ast_elem(), context()) -> e_ast_elem().
fill_offsets_stmt(#e_function{vars = Old} = S, Ctx) ->
	S#e_function{vars = fill_var_offsets(Old, Ctx)};
fill_offsets_stmt(#e_struct{fields = Old} = S, Ctx) ->
	S#e_struct{fields = fill_var_offsets(Old, Ctx)};
fill_offsets_stmt(Any, _) ->
	Any.

-spec fill_var_offsets(#e_vars{}, context()) -> #e_vars{}.
fill_var_offsets(#e_vars{names = Names, type_map = TypeMap} = Old, Ctx) ->
	TypeList = get_kvs_by_names(Names, TypeMap),
	{Size, Align, OffsetMap} = size_and_offsets(TypeList, {0, 0, #{}}, Ctx),
	Old#e_vars{offset_map = OffsetMap, size = Size, align = Align}.

-spec size_of_struct(#e_struct{}, context()) -> non_neg_integer().
size_of_struct(#e_struct{fields = #e_vars{size = Size}}, _) when Size > 0 ->
	Size;
size_of_struct(#e_struct{fields = #e_vars{names = FieldNames, type_map = TypeMap}}, Ctx) ->
	FieldTypeList = get_kvs_by_names(FieldNames, TypeMap),
	{Size, _, _} = size_and_offsets(FieldTypeList, {0, 0, #{}}, Ctx),
	Size.

-spec align_of_struct(#e_struct{}, context()) -> non_neg_integer().
align_of_struct(#e_struct{fields = #e_vars{align = Align}}, _) when Align > 0 ->
	Align;
align_of_struct(#e_struct{fields = #e_vars{type_map = TypeMap}}, Ctx) ->
	maps:fold(fun(_, Type, Align) -> erlang:max(align_of(Type, Ctx), Align) end, 0, TypeMap).

-spec get_kvs_by_names([atom()], #{atom() => any()}) -> [{atom(), any()}].
get_kvs_by_names(Names, Map) ->
	Values = e_util:get_values_by_keys(Names, Map),
	lists:zip(Names, Values).

-spec size_and_offsets([{atom(), e_type()}], R, context()) -> R
	when R :: {Size :: integer(), Align :: integer(), OffsetMap :: #{atom() => integer()}}.

size_and_offsets([{Name, Type} | Rest], {CurrentOffset, MaxAlign, OffsetMap}, Ctx) ->
	FieldSize = size_of(Type, Ctx),
	NextOffset = CurrentOffset + FieldSize,
	CurrentAlign = align_of(Type, Ctx),
	Align = erlang:max(MaxAlign, CurrentAlign),
	case CurrentOffset rem CurrentAlign =/= 0 of
		true ->
			Offset = fix_offset(CurrentOffset, NextOffset, CurrentAlign),
			size_and_offsets(Rest, {Offset + FieldSize, Align, OffsetMap#{Name => Offset}}, Ctx);
		false ->
			size_and_offsets(Rest, {NextOffset, Align, OffsetMap#{Name => CurrentOffset}}, Ctx)
	end;
size_and_offsets([], {CurrentOffset, Align, OffsetMap}, _) ->
	{e_util:fill_unit_pessi(CurrentOffset, Align), Align, OffsetMap}.

-spec fix_offset(integer(), integer(), integer()) -> integer().
fix_offset(CurrentOffset, NextOffset, TargetGap) ->
	case e_util:cut_extra(NextOffset, TargetGap) > e_util:cut_extra(CurrentOffset, TargetGap) of
		true ->
			e_util:fill_unit_opti(CurrentOffset, TargetGap);
		false ->
			CurrentOffset
	end.

-spec size_of(e_type(), context()) -> non_neg_integer().
size_of(#e_array_type{elem_type = T, length = Len}, Ctx) ->
	size_of(T, Ctx) * Len;
size_of(#e_fn_type{}, {_, PointerWidth}) ->
	PointerWidth;
size_of(#e_basic_type{p_depth = N}, {_, PointerWidth}) when N > 0 ->
	PointerWidth;
size_of(#e_basic_type{class = struct} = S, {StructMap, _} = Ctx) ->
	size_of_struct(e_util:get_struct_from_type(S, StructMap), Ctx);
size_of(#e_basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
	e_util:primitive_size_of(Tag, PointerWidth);
size_of(Any, _) ->
	e_util:ethrow(element(2, Any), "invalid type ~p on sizeof", [Any]).

-spec align_of(e_type(), context()) -> non_neg_integer().
align_of(#e_array_type{elem_type = T}, Ctx) ->
	align_of(T, Ctx);
align_of(#e_fn_type{}, {_, PointerWidth}) ->
	PointerWidth;
align_of(#e_basic_type{p_depth = N}, {_, PointerWidth}) when N > 0 ->
	PointerWidth;
align_of(#e_basic_type{class = struct} = S, {StructMap, _} = Ctx) ->
	align_of_struct(e_util:get_struct_from_type(S, StructMap), Ctx);
align_of(#e_basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
	e_util:primitive_size_of(Tag, PointerWidth);
align_of(Any, _) ->
	e_util:ethrow(element(2, Any), "invalid type ~p on alignof", [Any]).

