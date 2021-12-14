-module(e_size).
-export([expand_sizeof/2, expand_sizeof_in_exprs/2, fill_struct_info/2]).

-include("e_record_definition.hrl").

-type context() :: {struct_type_map(), non_neg_integer()}.

-spec expand_sizeof(e_ast(), context()) -> e_ast().
expand_sizeof([#function{stmts = Expressions} = F | Rest], Context) ->
    [F#function{stmts = expand_sizeof_in_exprs(Expressions, Context)} | expand_sizeof(Rest, Context)];
expand_sizeof([#struct{field_default_value_map = FieldDefaults} = S | Rest], Context) ->
    [S#struct{field_default_value_map = expand_sizeof_in_map(FieldDefaults, Context)} | expand_sizeof(Rest, Context)];
expand_sizeof([], _) ->
    [].

expand_sizeof_in_map(Map, Context) ->
    maps:map(fun (_, V1) -> expand_sizeof_in_expr(V1, Context) end, Map).

-spec expand_sizeof_in_exprs(e_ast(), context()) -> [e_expr()].
expand_sizeof_in_exprs(Expressions, Context) ->
    e_util:expr_map(fun (E) -> expand_sizeof_in_expr(E, Context) end, Expressions).

-spec expand_sizeof_in_expr(e_expr(), context()) -> e_expr().
expand_sizeof_in_expr(#sizeof_expr{type = T, line = Line}, Context) ->
    try {integer, Line, size_of(T, Context)} catch
        I ->
            throw({Line, I})
    end;
expand_sizeof_in_expr(#op2_expr{operand1 = Operand1, operand2 = Operand2} = O, Context) ->
    O#op2_expr{operand1 = expand_sizeof_in_expr(Operand1, Context), operand2 = expand_sizeof_in_expr(Operand2, Context)};
expand_sizeof_in_expr(#op1_expr{operand = Operand} = O, Context) ->
    O#op1_expr{operand = expand_sizeof_in_expr(Operand, Context)};
expand_sizeof_in_expr(#struct_init_expr{field_value_map = ExprMap} = Si, Context) ->
    Si#struct_init_expr{field_value_map = expand_sizeof_in_map(ExprMap, Context)};
expand_sizeof_in_expr(#array_init_expr{elements = Elements} = Ai, Context) ->
    Ai#array_init_expr{elements = expand_sizeof_in_exprs(Elements, Context)};
expand_sizeof_in_expr(Any, _) ->
    Any.

%% calculate struct size and collect field offsets.
%%
%% In the current algorithm, the size of the same struct will be calculated for multiple times, which is not necessary.
%% But the code is beautiful, so I will just keep it as it is now.
%% use a process to hold the calculated struct info when the speed really becomes a problem.
-spec fill_struct_info(e_ast(), context()) -> e_ast().
fill_struct_info(AST, {_, PointerWidth} = Context) ->
    %% struct definition are only allowed in top level of an AST.
    AST1 = lists:map(fun (E) -> fill_struct_size(E, Context) end, AST),
    {_, StructMap1} = e_util:make_function_and_struct_map_from_ast(AST1),
    lists:map(fun (E) -> fill_struct_offsets(E, {StructMap1, PointerWidth}) end, AST1).

-spec fill_struct_size(e_expr(), context()) -> e_expr().
fill_struct_size(#struct{} = S, Context) ->
    S#struct{size = size_of_struct(S, Context)};
fill_struct_size(Any, _) ->
    Any.

-spec fill_struct_offsets(e_expr(), context()) -> e_expr().
fill_struct_offsets(#struct{} = S, Context) ->
    S#struct{field_offset_map = offset_of_struct(S, Context)};
fill_struct_offsets(Any, _) ->
    Any.

-spec offset_of_struct(#struct{}, context()) -> #{atom() := integer()}.
offset_of_struct(#struct{field_names = FieldNames, field_type_map = FieldTypes}, Context) ->
    FieldTypeList = get_kvs_by_refs(FieldNames, FieldTypes),
    {_, OffsetMap} = size_of_struct_fields(FieldTypeList, 0, #{}, Context),
    OffsetMap.

-spec size_of_struct(#struct{}, context()) -> non_neg_integer().
size_of_struct(#struct{size = Size}, _) when is_integer(Size), Size > 0 ->
    Size;
size_of_struct(#struct{field_names = Names, field_type_map = Types}, Context) ->
    FieldTypeList = get_kvs_by_refs(Names, Types),
    {Size, _} = size_of_struct_fields(FieldTypeList, 0, #{}, Context),
    Size.

-spec get_kvs_by_refs([#variable_reference{}], #{atom() := any()}) -> [{atom(), any()}].
get_kvs_by_refs(RefList, Map) ->
    Keys = e_util:names_of_var_refs(RefList),
    Values = e_util:get_values_by_keys(Keys, Map),
    lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
-spec size_of_struct_fields([{atom(), e_type()}], integer(), OffsetMap, context()) -> {integer(), OffsetMap}
        when OffsetMap :: #{atom() := integer()}.
size_of_struct_fields([{FieldName, FieldType} | Rest], CurrentOffset, OffsetMap, {_, PointerWidth} = Context) ->
    FieldSize = size_of(FieldType, Context),
    NextOffset = CurrentOffset + FieldSize,
    case CurrentOffset rem PointerWidth =/= 0 of
        true ->
            OffsetFixed = fix_struct_field_offset(CurrentOffset, NextOffset, PointerWidth),
            size_of_struct_fields(Rest, OffsetFixed + FieldSize, OffsetMap#{FieldName => OffsetFixed}, Context);
        false ->
            size_of_struct_fields(Rest, NextOffset, OffsetMap#{FieldName => CurrentOffset}, Context)
    end;
size_of_struct_fields([], CurrentOffset, OffsetMap, _) ->
    {CurrentOffset, OffsetMap}.

-spec fix_struct_field_offset(integer(), integer(), integer()) -> integer().
fix_struct_field_offset(CurrentOffset, NextOffset, PointerWidth) ->
    case e_util:cut_extra(NextOffset, PointerWidth) > e_util:cut_extra(CurrentOffset, PointerWidth) of
        true ->
            e_util:fill_offset(CurrentOffset, PointerWidth);
        false ->
            CurrentOffset
    end.

-spec size_of(e_type(), context()) -> non_neg_integer().
size_of(#array_type{elem_type = T, length = Len}, {_, PointerWidth} = Context) ->
    ElementSize = size_of(T, Context),
    FixedSize = case ElementSize < PointerWidth of
                    true ->
                        case PointerWidth rem ElementSize of
                            0 ->
                                ElementSize;
                            _ ->
                                PointerWidth
                        end;
                    false ->
                        e_util:fill_to_pointer_width(ElementSize, PointerWidth)
                end,
    FixedSize * Len;
size_of(#basic_type{p_depth = N}, {_, PointerWidth}) when N > 0 ->
    PointerWidth;
size_of(#basic_type{class = struct, tag = Tag}, {StructMap, _} = Context) ->
    case maps:find(Tag, StructMap) of
        {ok, S} ->
            size_of_struct(S, Context);
        error ->
            throw(e_util:fmt("~s is not found", [Tag]))
    end;
size_of(#basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
    case e_util:primitive_size_of(Tag) of
        pointerSize ->
            PointerWidth;
        V when is_integer(V) ->
            V
    end;
size_of(#function_type{}, {_, PointerWidth}) ->
    PointerWidth;
size_of(A, _) ->
    throw(e_util:fmt("invalid type ~p on sizeof", [A])).
