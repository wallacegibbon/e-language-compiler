-module(e_size).
-export([expand_sizeof_in_ast/2, expand_sizeof_in_stmts/2, fill_struct_info/2]).
-include("e_record_definition.hrl").

-type context() :: {e_struct_type_map(), non_neg_integer()}.

-spec expand_sizeof_in_ast(e_ast(), context()) -> e_ast().
expand_sizeof_in_ast([#e_function{stmts = Stmts} = Fn | Rest], Ctx) ->
	[Fn#e_function{stmts = expand_sizeof_in_stmts(Stmts, Ctx)} | expand_sizeof_in_ast(Rest, Ctx)];
expand_sizeof_in_ast([#e_struct{field_default_value_map = FieldDefaults} = S | Rest], Ctx) ->
	[S#e_struct{field_default_value_map = expand_sizeof_in_map(FieldDefaults, Ctx)} | expand_sizeof_in_ast(Rest, Ctx)];
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

-spec fill_struct_info(e_ast(), context()) -> e_ast().
fill_struct_info(AST, {_, PointerWidth} = Ctx) ->
	%% struct definition are only allowed in top level of an AST.
	AST1 = lists:map(fun(E) -> fill_struct_size_and_align(E, Ctx) end, AST),
	{_, StructMap1} = e_util:make_function_and_struct_map_from_ast(AST1),
	lists:map(fun(E) -> fill_struct_offsets(E, {StructMap1, PointerWidth}) end, AST1).

-spec fill_struct_size_and_align(e_ast_elem(), context()) -> e_ast_elem().
fill_struct_size_and_align(#e_struct{} = S, Ctx) ->
	S#e_struct{size = size_of_struct(S, Ctx), align = align_of_struct(S, Ctx)};
fill_struct_size_and_align(Any, _) ->
	Any.

-spec fill_struct_offsets(e_ast_elem(), context()) -> e_ast_elem().
fill_struct_offsets(#e_struct{field_names = FieldNames, field_type_map = Types} = S, Ctx) ->
	FieldTypeList = get_kvs_by_refs(FieldNames, Types),
	{_, OffsetMap} = size_of_struct_fields(FieldTypeList, {0, #{}}, Ctx),
	S#e_struct{field_offset_map = OffsetMap};
fill_struct_offsets(Any, _) ->
	Any.

-spec size_of_struct(#e_struct{}, context()) -> non_neg_integer().
size_of_struct(#e_struct{size = Size}, _) when Size > 0 ->
	Size;
size_of_struct(#e_struct{field_names = FieldNames, field_type_map = Types}, Ctx) ->
	FieldTypeList = get_kvs_by_refs(FieldNames, Types),
	{Size, _} = size_of_struct_fields(FieldTypeList, {0, #{}}, Ctx),
	Size.

-spec align_of_struct(#e_struct{}, context()) -> non_neg_integer().
align_of_struct(#e_struct{align = Align}, _) when Align > 0 ->
	Align;
align_of_struct(#e_struct{field_type_map = Types}, Ctx) ->
	maps:fold(fun(_, Type, Align) -> erlang:max(align_of(Type, Ctx), Align) end, 0, Types).

-spec get_kvs_by_refs([#e_varref{}], #{atom() := any()}) -> [{atom(), any()}].
get_kvs_by_refs(RefList, Map) ->
	Keys = e_util:names_of_var_refs(RefList),
	Values = e_util:get_values_by_keys(Keys, Map),
	lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
-spec size_of_struct_fields([{atom(), e_type()}], R, context()) -> R when R :: {integer(), #{atom() := integer()}}.
size_of_struct_fields([{Name, Type} | Rest], {CurrentOffset, OffsetMap}, Ctx) ->
	FieldSize = size_of(Type, Ctx),
	NextOffset = CurrentOffset + FieldSize,
	Align = align_of(Type, Ctx),
	case CurrentOffset rem Align =/= 0 of
		true ->
			Offset = fix_struct_field_offset(CurrentOffset, NextOffset, Align),
			size_of_struct_fields(Rest, {Offset + FieldSize, OffsetMap#{Name => Offset}}, Ctx);
		false ->
			size_of_struct_fields(Rest, {NextOffset, OffsetMap#{Name => CurrentOffset}}, Ctx)
	end;
size_of_struct_fields([], {CurrentOffset, OffsetMap}, _) ->
	{CurrentOffset, OffsetMap}.

-spec fix_struct_field_offset(integer(), integer(), integer()) -> integer().
fix_struct_field_offset(CurrentOffset, NextOffset, TargetGap) ->
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
size_of(#e_basic_type{class = struct, tag = Tag, line = Line}, {StructMap, _} = Ctx) ->
	case maps:find(Tag, StructMap) of
		{ok, S} ->
			size_of_struct(S, Ctx);
		error ->
			e_util:ethrow(Line, "~s is not found", [Tag])
	end;
size_of(#e_basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
	case e_util:primitive_size_of(Tag) of
		pointer_size ->
			PointerWidth;
		V ->
			V
	end;
size_of(Any, _) ->
	e_util:ethrow(element(2, Any), "invalid type ~p on sizeof", [Any]).

-spec align_of(e_type(), context()) -> non_neg_integer().
align_of(#e_array_type{elem_type = T}, Ctx) ->
	align_of(T, Ctx);
align_of(#e_fn_type{}, {_, PointerWidth}) ->
	PointerWidth;
align_of(#e_basic_type{p_depth = N}, {_, PointerWidth}) when N > 0 ->
	PointerWidth;
align_of(#e_basic_type{class = struct, tag = Tag, line = Line}, {StructMap, _} = Ctx) ->
	case maps:find(Tag, StructMap) of
		{ok, S} ->
			align_of_struct(S, Ctx);
		error ->
			e_util:ethrow(Line, "~s is not found", [Tag])
	end;
align_of(#e_basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
	case e_util:primitive_size_of(Tag) of
		pointer_size ->
			PointerWidth;
		V ->
			V
	end;
align_of(Any, _) ->
	e_util:ethrow(element(2, Any), "invalid type ~p on alignof", [Any]).

