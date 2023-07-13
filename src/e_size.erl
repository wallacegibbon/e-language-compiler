-module(e_size).
-export([expand_sizeof/2, expand_sizeof_in_exprs/2, fill_struct_info/2]).

-include("e_record_definition.hrl").

-type context() :: {struct_type_map(), non_neg_integer()}.

-spec expand_sizeof(e_ast(), context()) -> e_ast().
expand_sizeof([#function{stmts = Exprs} = F | Rest], Ctx) ->
	[F#function{stmts = expand_sizeof_in_exprs(Exprs, Ctx)} | expand_sizeof(Rest, Ctx)];

expand_sizeof([#struct{field_default_value_map = FieldDefaults} = S | Rest], Ctx) ->
	[S#struct{field_default_value_map = expand_sizeof_in_map(FieldDefaults, Ctx)} | expand_sizeof(Rest, Ctx)];

expand_sizeof([], _) ->
	[].


expand_sizeof_in_map(Map, Ctx) ->
	maps:map(fun (_, V1) -> expand_sizeof_in_expr(V1, Ctx) end, Map).


-spec expand_sizeof_in_exprs(e_ast(), context()) -> [e_expr()].
expand_sizeof_in_exprs(Exprs, Ctx) ->
	e_util:expr_map(fun (E) -> expand_sizeof_in_expr(E, Ctx) end, Exprs).


-spec expand_sizeof_in_expr(e_expr(), context()) -> e_expr().
expand_sizeof_in_expr(#e_expr{tag = {sizeof, T}, line = Line}, Ctx) ->
	try
		{integer, Line, size_of(T, Ctx)}
	catch
	I ->
		throw({Line, I})
	end;

expand_sizeof_in_expr(#e_expr{data = Data} = E, Ctx) ->
	E#e_expr{data = lists:map(fun (O) -> expand_sizeof_in_expr(O, Ctx) end, Data)};
expand_sizeof_in_expr(#struct_init_expr{field_value_map = ExprMap} = S, Ctx) ->
	S#struct_init_expr{field_value_map = expand_sizeof_in_map(ExprMap, Ctx)};

expand_sizeof_in_expr(#array_init_expr{elements = Elements} = A, Ctx) ->
	A#array_init_expr{elements = expand_sizeof_in_exprs(Elements, Ctx)};

expand_sizeof_in_expr(Any, _) ->
	Any.


%% calculate struct size and collect field offsets.
%%
%% In the current algorithm, the size of the same struct will be calculated
%% for multiple times, which is not necessary.
%% But the code is beautiful, so I will just keep it as it is now.
%% use a process to hold the calculated struct info
%% when the speed really becomes a problem.
-spec fill_struct_info(e_ast(), context()) -> e_ast().
fill_struct_info(AST, {_, PointerWidth} = Ctx) ->
	%% struct definition are only allowed in top level of an AST.
	AST1 = lists:map(fun (E) -> fill_struct_size(E, Ctx) end, AST),
	{_, StructMap1} = e_util:make_function_and_struct_map_from_ast(AST1),
	lists:map(fun (E) -> fill_struct_offsets(E, {StructMap1, PointerWidth}) end, AST1).

-spec fill_struct_size(e_ast_elem(), context()) -> e_ast_elem().
fill_struct_size(#struct{} = S, Ctx) ->
	S#struct{size = size_of_struct(S, Ctx)};
fill_struct_size(Any, _) ->
	Any.

-spec fill_struct_offsets(e_ast_elem(), context()) -> e_ast_elem().
fill_struct_offsets(#struct{} = S, Ctx) ->
	S#struct{field_offset_map = offset_of_struct(S, Ctx)};
fill_struct_offsets(Any, _) ->
	Any.

-spec offset_of_struct(#struct{}, context()) -> #{atom() := integer()}.
offset_of_struct(#struct{field_names = Names, field_type_map = FieldTypes}, Ctx) ->
	FieldTypeList = get_kvs_by_refs(Names, FieldTypes),
	{_, OffsetMap} = size_of_struct_fields(FieldTypeList, 0, #{}, Ctx),
	OffsetMap.

-spec size_of_struct(#struct{}, context()) -> non_neg_integer().
size_of_struct(#struct{size = Size}, _) when Size > 0 ->
	Size;
size_of_struct(#struct{field_names = Names, field_type_map = Types}, Ctx) ->
	FieldTypeList = get_kvs_by_refs(Names, Types),
	{Size, _} = size_of_struct_fields(FieldTypeList, 0, #{}, Ctx),
	Size.

-spec get_kvs_by_refs([#var_ref{}], #{atom() := any()}) -> [{atom(), any()}].
get_kvs_by_refs(RefList, Map) ->
	Keys = e_util:names_of_var_refs(RefList),
	Values = e_util:get_values_by_keys(Keys, Map),
	lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
-spec size_of_struct_fields([{atom(), e_type()}], integer(), OffsetMap, context())
	-> {integer(), OffsetMap}
	when OffsetMap :: #{atom() := integer()}.

size_of_struct_fields([{FieldName, FieldType} | Rest], CurrentOffset, OffsetMap, {_, PointerWidth} = Ctx) ->
	FieldSize = size_of(FieldType, Ctx),
	NextOffset = CurrentOffset + FieldSize,
	case CurrentOffset rem PointerWidth =/= 0 of
	true ->
		Offset = fix_struct_field_offset(CurrentOffset, NextOffset, PointerWidth),
		size_of_struct_fields(Rest, Offset + FieldSize, OffsetMap#{FieldName => Offset}, Ctx);
	false ->
		size_of_struct_fields(Rest, NextOffset, OffsetMap#{FieldName => CurrentOffset}, Ctx)
	end;

size_of_struct_fields([], CurrentOffset, OffsetMap, _) ->
	{CurrentOffset, OffsetMap}.


-spec fix_struct_field_offset(integer(), integer(), integer()) -> integer().
fix_struct_field_offset(CurrentOffset, NextOffset, PointerWidth) ->
	case
		e_util:cut_extra(NextOffset, PointerWidth) >
		e_util:cut_extra(CurrentOffset, PointerWidth)
	of
	true ->
		e_util:fill_unit_opti(CurrentOffset, PointerWidth);
	false ->
		CurrentOffset
	end.


-spec size_of(e_type(), context()) -> non_neg_integer().
size_of(#array_type{elem_type = T, length = Len}, {_, PointerWidth} = Ctx) ->
	TotalSize = align_fix(size_of(T, Ctx), PointerWidth) * Len,
	align_fix(TotalSize, PointerWidth);
size_of(#basic_type{p_depth = N}, {_, PointerWidth}) when N > 0 ->
	PointerWidth;
size_of(#basic_type{class = struct, tag = Tag, line = Line}, {StructMap, _} = Ctx) ->
	case maps:find(Tag, StructMap) of
	{ok, S} ->
		size_of_struct(S, Ctx);
	error ->
		e_util:ethrow(Line, "~s is not found", [Tag])
	end;
size_of(#basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
	case e_util:primitive_size_of(Tag) of
	pointer_size ->
		PointerWidth;
	V when is_integer(V) ->
		V
	end;
size_of(#fn_type{}, {_, PointerWidth}) ->
	PointerWidth;
size_of(A, _) ->
	Line = element(1, A),
	e_util:ethrow(Line, "invalid type ~p on sizeof", [A]).

-spec align_fix(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
align_fix(Size, Unit) when Size < Unit, Unit rem Size =:= 0 ->
	Size;
align_fix(Size, Unit) when Size < Unit ->
	Unit;
align_fix(Size, Unit) ->
	e_util:fill_unit_pessi(Size, Unit).

