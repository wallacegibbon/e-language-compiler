-module(ecompilerFillSize).

-export([expandSizeOf/2, expandSizeofInExpressions/2, fillStructInformation/2]).

-include("ecompilerFrameDef.hrl").

-spec expandSizeOf(eAST(), compilePassCtx1()) -> eAST().
expandSizeOf([#function{statements = Expressions} = F | Rest], Context) ->
    [F#function{statements = expandSizeofInExpressions(Expressions, Context)} | expandSizeOf(Rest, Context)];
expandSizeOf([#struct{fieldDefaultValueMap = FieldDefaults} = S | Rest], Context) ->
    [S#struct{fieldDefaultValueMap = expandSizeofInMap(FieldDefaults, Context)} | expandSizeOf(Rest, Context)];
expandSizeOf([], _) ->
    [].

expandSizeofInMap(Map, Context) ->
    maps:map(fun (_, V1) -> expandSizeofInExpression(V1, Context) end, Map).

-spec expandSizeofInExpressions(eAST(), compilePassCtx1()) -> [eExpression()].
expandSizeofInExpressions(Expressions, Context) ->
    ecompilerUtil:expressionMap(fun (E) -> expandSizeofInExpression(E, Context) end, Expressions).

-spec expandSizeofInExpression(eExpression(), compilePassCtx1()) -> eExpression().
expandSizeofInExpression(#sizeofExpression{type = T, line = Line}, Context) ->
    try {integer, Line, sizeOf(T, Context)} catch
        I ->
            throw({Line, I})
    end;
expandSizeofInExpression(#operatorExpression2{operand1 = Operand1, operand2 = Operand2} = O, Context) ->
    O#operatorExpression2{operand1 = expandSizeofInExpression(Operand1, Context), operand2 = expandSizeofInExpression(Operand2, Context)};
expandSizeofInExpression(#operatorExpression1{operand = Operand} = O, Context) ->
    O#operatorExpression1{operand = expandSizeofInExpression(Operand, Context)};
expandSizeofInExpression(#structInitializeExpression{fieldValueMap = ExprMap} = Si, Context) ->
    Si#structInitializeExpression{fieldValueMap = expandSizeofInMap(ExprMap, Context)};
expandSizeofInExpression(#arrayInitializeExpression{elements = Elements} = Ai, Context) ->
    Ai#arrayInitializeExpression{elements = expandSizeofInExpressions(Elements, Context)};
expandSizeofInExpression(Any, _) ->
    Any.

%% calculate struct size and collect field offsets.
%%
%% In the current algorithm, the size of the same struct will be calculated for multiple times, which is not necessary.
%% But the code is beautiful, so I will just keep it as it is now.
%% use a process to hold the calculated struct info when the speed really becomes a problem.
-spec fillStructInformation(eAST(), compilePassCtx1()) -> eAST().
fillStructInformation(AST, {_, PointerWidth} = Context) ->
    %% struct definition are only allowed in top level of an AST.
    AST1 = lists:map(fun (E) -> fillStructSize(E, Context) end, AST),
    {_, StructMap1} = ecompilerUtil:makeFunctionAndStructMapFromAST(AST1),
    lists:map(fun (E) -> fillStructOffsets(E, {StructMap1, PointerWidth}) end, AST1).

-spec fillStructSize(eExpression(), compilePassCtx1()) -> eExpression().
fillStructSize(#struct{} = S, Context) ->
    S#struct{size = sizeOfStruct(S, Context)};
fillStructSize(Any, _) ->
    Any.

-spec fillStructOffsets(eExpression(), compilePassCtx1()) -> eExpression().
fillStructOffsets(#struct{} = S, Context) ->
    S#struct{fieldOffsetMap = offsetOfStruct(S, Context)};
fillStructOffsets(Any, _) ->
    Any.

-spec offsetOfStruct(#struct{}, compilePassCtx1()) -> #{atom() := integer()}.
offsetOfStruct(#struct{fieldNames = FieldNames, fieldTypeMap = FieldTypes}, Context) ->
    FieldTypeList = getKVsByReferences(FieldNames, FieldTypes),
    {_, OffsetMap} = sizeOfStructFields(FieldTypeList, 0, #{}, Context),
    OffsetMap.

-spec sizeOfStruct(#struct{}, compilePassCtx1()) -> non_neg_integer().
sizeOfStruct(#struct{size = Size}, _) when is_integer(Size), Size > 0 ->
    Size;
sizeOfStruct(#struct{fieldNames = Names, fieldTypeMap = Types}, Context) ->
    FieldTypeList = getKVsByReferences(Names, Types),
    {Size, _} = sizeOfStructFields(FieldTypeList, 0, #{}, Context),
    Size.

-spec getKVsByReferences([#variableReference{}], #{atom() := any()}) -> [{atom(), any()}].
getKVsByReferences(RefList, Map) ->
    Keys = ecompilerUtil:namesOfVariableReferences(RefList),
    Values = ecompilerUtil:getValuesByKeys(Keys, Map),
    lists:zip(Keys, Values).

%% this is the function that calculate size and offsets
-spec sizeOfStructFields([{atom(), eType()}], integer(), OffsetMap, compilePassCtx1()) -> {integer(), OffsetMap}
        when OffsetMap :: #{atom() := integer()}.
sizeOfStructFields([{Fname, Ftype} | Rest], CurrentOffset, OffsetMap, {_, PointerWidth} = Context) ->
    FieldSize = sizeOf(Ftype, Context),
    NextOffset = CurrentOffset + FieldSize,
    case CurrentOffset rem PointerWidth =/= 0 of
        true ->
            OffsetFixed = fixStructFieldOffset(CurrentOffset, NextOffset, PointerWidth),
            sizeOfStructFields(Rest, OffsetFixed + FieldSize, OffsetMap#{Fname => OffsetFixed}, Context);
        false ->
            sizeOfStructFields(Rest, NextOffset, OffsetMap#{Fname => CurrentOffset}, Context)
    end;
sizeOfStructFields([], CurrentOffset, OffsetMap, _) ->
    {CurrentOffset, OffsetMap}.

-spec fixStructFieldOffset(integer(), integer(), integer()) -> integer().
fixStructFieldOffset(CurrentOffset, NextOffset, PointerWidth) ->
    case ecompilerUtil:cutExtra(NextOffset, PointerWidth) > ecompilerUtil:cutExtra(CurrentOffset, PointerWidth) of
        true ->
            ecompilerUtil:fillOffset(CurrentOffset, PointerWidth);
        false ->
            CurrentOffset
    end.

-spec sizeOf(eType(), compilePassCtx1()) -> non_neg_integer().
sizeOf(#arrayType{elemtype = T, length = Len}, {_, PointerWidth} = Context) ->
    ElementSize = sizeOf(T, Context),
    FixedSize = case ElementSize < PointerWidth of
                    true ->
                        case PointerWidth rem ElementSize of
                            0 ->
                                ElementSize;
                            _ ->
                                PointerWidth
                        end;
                    false ->
                        ecompilerUtil:fillToPointerWidth(ElementSize, PointerWidth)
                end,
    FixedSize * Len;
sizeOf(#basicType{pdepth = N}, {_, PointerWidth}) when N > 0 ->
    PointerWidth;
sizeOf(#basicType{class = struct, tag = Tag}, {StructMap, _} = Context) ->
    case maps:find(Tag, StructMap) of
        {ok, S} ->
            sizeOfStruct(S, Context);
        error ->
            throw(ecompilerUtil:fmt("~s is not found", [Tag]))
    end;
sizeOf(#basicType{class = C, tag = Tag}, {_, PointerWidth}) when C =:= integer; C =:= float ->
    case ecompilerUtil:primitiveSizeOf(Tag) of
        pwidth ->
            PointerWidth;
        V when is_integer(V) ->
            V
    end;
sizeOf(#functionType{}, {_, PointerWidth}) ->
    PointerWidth;
sizeOf(A, _) ->
    throw(ecompilerUtil:fmt("invalid type ~p on sizeof", [A])).
