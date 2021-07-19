-module(ecompilerFillSize).

-export([expandSizeOf/2, expandSizeofInExpressions/2, fillStructInformation/2]).

-include("./ecompilerFrameDef.hrl").

-spec expandSizeOf(eAST(), compilePassCtx1()) -> eAST().
expandSizeOf([#function{exprs = Expressions} = F | Rest], Ctx) ->
    [F#function{exprs = expandSizeofInExpressions(Expressions, Ctx)} | expandSizeOf(Rest, Ctx)];
expandSizeOf([#struct{field_defaults = FieldDefaults} = S | Rest], Ctx) ->
    [S#struct{field_defaults = prvExpandSizeofInMap(FieldDefaults, Ctx)} | expandSizeOf(Rest, Ctx)];
expandSizeOf([], _) ->
    [].

prvExpandSizeofInMap(Map, Ctx) ->
    maps:map(fun (_, V1) -> prvExpandSizeofInExpression(V1, Ctx) end, Map).

-spec expandSizeofInExpressions(eAST(), compilePassCtx1()) -> [eExpression()].
expandSizeofInExpressions(Expressions, Ctx) ->
    ecompilerUtil:expressionMap(fun (E) -> prvExpandSizeofInExpression(E, Ctx) end, Expressions).

-spec prvExpandSizeofInExpression(eExpression(), compilePassCtx1()) -> eExpression().
prvExpandSizeofInExpression(#sizeof{type = T, line = Line}, Ctx) ->
    try {integer, Line, prvSizeOf(T, Ctx)} catch
        I ->    throw({Line, I})
    end;
prvExpandSizeofInExpression(#op2{op1 = Operand1, op2 = Operand2} = O, Ctx) ->
    O#op2{op1 = prvExpandSizeofInExpression(Operand1, Ctx), op2 = prvExpandSizeofInExpression(Operand2, Ctx)};
prvExpandSizeofInExpression(#op1{operand = Operand} = O, Ctx) ->
    O#op1{operand = prvExpandSizeofInExpression(Operand, Ctx)};
prvExpandSizeofInExpression(#struct_init{field_values = ExprMap} = Si, Ctx) ->
    Si#struct_init{field_values = prvExpandSizeofInMap(ExprMap, Ctx)};
prvExpandSizeofInExpression(#array_init{elements = Elements} = Ai, Ctx) ->
    Ai#array_init{elements = expandSizeofInExpressions(Elements, Ctx)};
prvExpandSizeofInExpression(Any, _) ->
    Any.

%% calculate struct size and collect field offsets.
%%
%% In the current algorithm, the size of the same struct will be calculated for multiple times, which is not necessary.
%% But the code is beautiful, so I will just keep it as it is now.
%% use a process to hold the calculated struct info when the speed really becomes a problem.
-spec fillStructInformation(eAST(), compilePassCtx1()) -> eAST().
fillStructInformation(AST, {_, PointerWidth} = Ctx) ->
    %% struct definition are only allowed in top level of an AST.
    Ast1 = lists:map(fun (E) -> prvFillStructSize(E, Ctx) end, AST),
    {_, StructMap1} = ecompilerUtil:makeFunctionAndStructMapFromAST(Ast1),
    lists:map(fun (E) -> prvFillStructOffsets(E, {StructMap1, PointerWidth}) end, Ast1).

-spec prvFillStructSize(eExpression(), compilePassCtx1()) -> eExpression().
prvFillStructSize(#struct{} = S, Ctx) ->        S#struct{size = prvSizeOfStruct(S, Ctx)};
prvFillStructSize(Any, _) ->                    Any.

-spec prvFillStructOffsets(eExpression(), compilePassCtx1()) -> eExpression().
prvFillStructOffsets(#struct{} = S, Ctx) ->     S#struct{field_offsets = prvOffsetOfStruct(S, Ctx)};
prvFillStructOffsets(Any, _) ->                 Any.

-spec prvOffsetOfStruct(#struct{}, compilePassCtx1()) -> #{atom() := integer()}.
prvOffsetOfStruct(#struct{field_names = FieldNames, field_types = FieldTypes}, Ctx) ->
    FieldTypeList = prvGetKVsByReferences(FieldNames, FieldTypes),
    {_, OffsetMap} = prvSizeOfStructFields(FieldTypeList, 0, #{}, Ctx),
    OffsetMap.

-spec prvSizeOfStruct(#struct{}, compilePassCtx1()) -> non_neg_integer().
prvSizeOfStruct(#struct{size = Size}, _) when is_integer(Size) ->
    Size;
prvSizeOfStruct(#struct{field_names = Names, field_types = Types}, Ctx) ->
    FieldTypeList = prvGetKVsByReferences(Names, Types),
    {Size, _} = prvSizeOfStructFields(FieldTypeList, 0, #{}, Ctx),
    Size.

-spec prvGetKVsByReferences([#varref{}], #{atom() := any()}) -> [{atom(), any()}].
prvGetKVsByReferences(RefList, Map) ->
    Keys = ecompilerUtil:namesOfVariableReferences(RefList),
    Values = ecompilerUtil:getValuesByKeys(Keys, Map),
    lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
-spec prvSizeOfStructFields([{atom(), eType()}], integer(), OffsetMap, compilePassCtx1()) -> {integer(), OffsetMap}
        when OffsetMap :: #{atom() := integer()}.
prvSizeOfStructFields([{Fname, Ftype} | Rest], CurrentOffset, OffsetMap, {_, PointerWidth} = Ctx) ->
    FieldSize = prvSizeOf(Ftype, Ctx),
    NextOffset = CurrentOffset + FieldSize,
    case CurrentOffset rem PointerWidth =/= 0 of
        true ->
            OffsetFixed = prvFixStructFieldOffset(CurrentOffset, NextOffset, PointerWidth),
            prvSizeOfStructFields(Rest, OffsetFixed + FieldSize, OffsetMap#{Fname => OffsetFixed}, Ctx);
        _ ->
            prvSizeOfStructFields(Rest, NextOffset, OffsetMap#{Fname => CurrentOffset}, Ctx)
    end;
prvSizeOfStructFields([], CurrentOffset, OffsetMap, _) ->
    {CurrentOffset, OffsetMap}.

-spec prvFixStructFieldOffset(integer(), integer(), integer()) -> integer().
prvFixStructFieldOffset(CurrentOffset, NextOffset, PointerWidth) ->
    case ecompilerUtil:cutExtra(NextOffset, PointerWidth) > ecompilerUtil:cutExtra(CurrentOffset, PointerWidth) of
        true ->     ecompilerUtil:fillOffset(CurrentOffset, PointerWidth);
        _ ->        CurrentOffset
    end.

-spec prvSizeOf(eType(), compilePassCtx1()) -> non_neg_integer().
prvSizeOf(#array_type{elemtype = T, len = Len}, {_, PointerWidth} = Ctx) ->
    ElemSize = prvSizeOf(T, Ctx),
    FixedSize = case ElemSize < PointerWidth of
                    true ->
                        case PointerWidth rem ElemSize of
                            0 ->    ElemSize;
                            _ ->    PointerWidth
                        end;
                    _ ->
                        ecompilerUtil:fillToPointerWidth(ElemSize, PointerWidth)
                end,
    FixedSize * Len;
prvSizeOf(#basic_type{pdepth = N}, {_, PointerWidth}) when N > 0 ->
    PointerWidth;
prvSizeOf(#basic_type{class = struct, tag = Tag}, {StructMap, _} = Ctx) ->
    case maps:find(Tag, StructMap) of
        {ok, S} ->      prvSizeOfStruct(S, Ctx);
        error ->        throw( ecompilerUtil:flatfmt("~s is not found", [Tag]) )
    end;
prvSizeOf(#basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
    case ecompilerUtil:primitiveSizeOf(Tag) of
        pwidth ->                   PointerWidth;
        V when is_integer(V) ->     V;
        _ ->                        throw( ecompilerUtil:flatfmt("primitiveSize(~s) is invalid", [Tag]) )
    end;
prvSizeOf(#fun_type{}, {_, PointerWidth}) ->
    PointerWidth;
prvSizeOf(A, _) ->
    throw(ecompilerUtil:flatfmt("invalid type ~p on sizeof", [A])).