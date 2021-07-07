-module(ecompiler_fillsize).

-export([expandSizeOf/2, expandSizeofInExpressions/2, fillStructInformation/2]).

-include("./ecompiler_frame.hrl").

expandSizeOf([#function{exprs = Exprs} = F | Rest], Ctx) ->
    [F#function{exprs = expandSizeofInExpressions(Exprs, Ctx)} | expandSizeOf(Rest, Ctx)];
expandSizeOf([#struct{field_defaults = FieldDefaults} = S | Rest], Ctx) ->
    [S#struct{field_defaults = prvExpandSizeofInMap(FieldDefaults, Ctx)} | expandSizeOf(Rest, Ctx)];
expandSizeOf([], _) ->
    [].

prvExpandSizeofInMap(Map, Ctx) ->
    maps:map(fun (_, V1) -> prvExpandSizeofInExpression(V1, Ctx) end, Map).

expandSizeofInExpressions(Exprs, Ctx) ->
    ecompiler_util:expressionMap(fun (E) -> prvExpandSizeofInExpression(E, Ctx) end, Exprs).

prvExpandSizeofInExpression(#sizeof{type = T, line = Line}, Ctx) ->
    try {integer, Line, prvSizeOf(T, Ctx)} catch
        I -> throw({Line, I})
    end;
prvExpandSizeofInExpression(#op2{op1 = Op1, op2 = Op2} = O, Ctx) ->
    O#op2{op1 = prvExpandSizeofInExpression(Op1, Ctx), op2 = prvExpandSizeofInExpression(Op2, Ctx)};
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
%% In the current algorithm, the size of the same struct will be calculated
%% for multiple times, which is not necessary. But the code is beautiful, so
%% I will just keep it as it is now.
%% use a process to hold the calculated struct info when the speed really
%% becomes a problem.
fillStructInformation(Ast, {_, PointerWidth} = Ctx) ->
    Ast1 = lists:map(fun (E) -> prvFillStructSize(E, Ctx) end, Ast),
    {_, StructMap1} = ecompiler_util:makeFunctionAndStructMapFromAST(Ast1),
    Ctx1 = {StructMap1, PointerWidth},
    Ast2 = lists:map(fun (E) -> prvFillStructOffsets(E, Ctx1) end, Ast1),
    Ast2.

prvFillStructSize(#struct{} = S, Ctx) -> S#struct{size = prvSizeOfStruct(S, Ctx)};
prvFillStructSize(Any, _) -> Any.

prvFillStructOffsets(#struct{} = S, Ctx) -> S#struct{field_offsets = prvOffsetOfStruct(S, Ctx)};
prvFillStructOffsets(Any, _) -> Any.

prvOffsetOfStruct(#struct{field_names = FieldNames, field_types = FieldTypes}, Ctx) ->
    FieldTypeList = prvGetKVsByReferences(FieldNames, FieldTypes),
    {_, OffsetMap} = prvSizeOfStructFields(FieldTypeList, 0, #{}, Ctx),
    OffsetMap.

prvSizeOfStruct(#struct{size = Size}, _) when is_integer(Size) ->
    Size;
prvSizeOfStruct(#struct{field_names = Names, field_types = Types}, Ctx) ->
    FieldTypeList = prvGetKVsByReferences(Names, Types),
    {Size, _} = prvSizeOfStructFields(FieldTypeList, 0, #{}, Ctx),
    Size.

prvGetKVsByReferences(RefList, Map) ->
    Keys = ecompiler_util:namesOfVariableReferences(RefList),
    Values = ecompiler_util:getValuesByKeys(Keys, Map),
    lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
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

prvFixStructFieldOffset(CurrentOffset, NextOffset, PointerWidth) ->
    case ecompiler_util:cutExtra(NextOffset, PointerWidth) > ecompiler_util:cutExtra(CurrentOffset, PointerWidth) of
        true ->
            ecompiler_util:fillOffset(CurrentOffset, PointerWidth);
        _ ->
            CurrentOffset
    end.

%%
prvSizeOf(#array_type{elemtype = T, len = Len}, {_, PointerWidth} = Ctx) ->
    ElemSize = prvSizeOf(T, Ctx),
    FixedSize = case ElemSize < PointerWidth of
                    true ->
                        case PointerWidth rem ElemSize of
                            0 ->
                                ElemSize;
                            _ ->
                                PointerWidth
                        end;
                    _ ->
                        ecompiler_util:fillToPointerWidth(ElemSize, PointerWidth)
                end,
    FixedSize * Len;
prvSizeOf(#basic_type{pdepth = N}, {_, PointerWidth}) when N > 0 ->
    PointerWidth;
prvSizeOf(#basic_type{class = struct, tag = Tag}, {StructMap, _} = Ctx) ->
    case maps:find(Tag, StructMap) of
        {ok, S} ->
            prvSizeOfStruct(S, Ctx);
        error ->
            throw(ecompiler_util:flatfmt("~s is not found", [Tag]))
    end;
prvSizeOf(#basic_type{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
    case ecompiler_util:primitiveSizeOf(Tag) of
        pwidth ->
            PointerWidth;
        V when is_integer(V) ->
            V;
        _ ->
            throw(ecompiler_util:flatfmt("primitiveSize(~s) is invalid", [Tag]))
    end;
prvSizeOf(#fun_type{}, {_, PointerWidth}) ->
    PointerWidth;
prvSizeOf(A, _) ->
    throw(ecompiler_util:flatfmt("invalid type ~p on sizeof", [A])).

