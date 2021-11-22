-module(ecompilerUtil).

-export([expressionToString/1, expressionMap/2, filterVariableReferenceInMap/2, fmt/2, getValuesByKeys/2, namesOfVariableDefinitions/1, namesOfVariableReferences/1, valueInList/2]).
-export([primitiveSizeOf/1, voidType/1]).
-export([cutExtra/2, fillOffset/2, fillToPointerWidth/2]).
-export([makeFunctionAndStructMapFromAST/1]).
-export([assert/2]).

-include_lib("eunit/include/eunit.hrl").
-include("ecompilerFrameDef.hrl").

%% when do simple conversions, this function can be used to avoid boilerplate
%% code for if, while, return, call..., so you can concentrate on operand1, operand2...
-spec expressionMap(fun ((eExpression()) -> eExpression()), [eExpression()]) -> eExpression().
expressionMap(Fn, [#ifStatement{condition = Cond, then = Then, else = Else} = If | Rest]) ->
    [If#ifStatement{condition = Fn(Cond), then = expressionMap(Fn, Then), else = expressionMap(Fn, Else)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [#whileStatement{condition = Cond, statements = Expressions} = While | Rest]) ->
    [While#whileStatement{condition = Fn(Cond), statements = expressionMap(Fn, Expressions)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [#callExpression{fn = Callee, args = Arguments} = FunctionCall | Rest]) ->
    [FunctionCall#callExpression{fn = Fn(Callee), args = expressionMap(Fn, Arguments)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [#returnStatement{expression = ReturnExpression} = Return | Rest]) ->
    [Return#returnStatement{expression = Fn(ReturnExpression)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [Any | Rest]) ->
    [Fn(Any) | expressionMap(Fn, Rest)];
expressionMap(_, []) ->
    [].

-spec expressionToString(eExpression()) -> string().
expressionToString(#ifStatement{condition = Cond, then = Then, else = Else}) ->
    io_lib:format("if (~s) ~s else ~s end", [expressionToString(Cond), expressionToString(Then), expressionToString(Else)]);
expressionToString(#whileStatement{condition = Cond, statements = Expressions}) ->
    io_lib:format("while (~s) ~s end", [expressionToString(Cond), expressionToString(Expressions)]);
expressionToString(#callExpression{fn = Callee, args = Arguments}) ->
    io_lib:format("(~s)(~s)", [expressionToString(Callee), expressionToString(Arguments)]);
expressionToString(#returnStatement{expression = ReturnExpression}) ->
    io_lib:format("return (~s)", [expressionToString(ReturnExpression)]);
expressionToString(#variableReference{name = Name}) ->
    io_lib:format("~s", [expressionToString(Name)]);
expressionToString(#operatorExpression2{operator = Operator, operand1 = Operand1, operand2 = Operand2}) ->
    io_lib:format("~s ~s ~s", [expressionToString(Operand1), Operator, expressionToString(Operand2)]);
expressionToString(#operatorExpression1{operator = Operator, operand = Operand}) ->
    io_lib:format("~s ~s", [expressionToString(Operand), Operator]);
expressionToString({ImmediateValue, _, Val}) when ImmediateValue =:= integer; ImmediateValue =:= float ->
    io_lib:format("~w", [Val]);
expressionToString({ImmediateValue, _, Val}) when ImmediateValue =:= string ->
    Val;
expressionToString(Any) ->
    Any.

-spec fmt(string(), [any()]) -> string().
fmt(FmtStr, Arguments) ->
    lists:flatten(io_lib:format(FmtStr, Arguments)).

-spec getValuesByKeys([atom()], #{atom() => any()}) -> [any()].
getValuesByKeys(Fields, Map) when is_map(Map) ->
    getValuesByKeys(Fields, Map, []).

-spec getValuesByKeys([atom()], #{atom() => any()}, [any()]) -> [any()].
getValuesByKeys([Field | Rest], Map, Result) ->
    getValuesByKeys(Rest, Map, [maps:get(Field, Map) | Result]);
getValuesByKeys([], _, Result) ->
    lists:reverse(Result).

-ifdef(EUNIT).

getValuesByKeys_test() ->
    ?assertEqual([3, 2], getValuesByKeys([a, b], #{c => 1, b => 2, a => 3})).

-endif.

-spec makeFunctionAndStructMapFromAST(any()) -> {functionTypeMap(), structTypeMap()}.
makeFunctionAndStructMapFromAST(AST) ->
    {Functions, Structs} = lists:partition(fun (A) -> element(1, A) =:= function end, AST),
    %% FunctionTypeMap stores function type only
    FunctionTypeMap = maps:from_list(lists:map(fun (#function{name = Name} = Fn) -> {Name, Fn#function.type} end, Functions)),
    StructMap = maps:from_list(lists:map(fun (#struct{name = Name} = S) -> {Name, S} end, Structs)),
    {FunctionTypeMap, StructMap}.

%% address calculations
-spec fillToPointerWidth(integer(), non_neg_integer()) -> integer().
fillToPointerWidth(Num, PointerWidth) ->
    (Num + PointerWidth - 1) div PointerWidth * PointerWidth.

-spec fillOffset(integer(), non_neg_integer()) -> integer().
fillOffset(Offset, PointerWidth) ->
    (Offset + PointerWidth) div PointerWidth * PointerWidth.

-spec cutExtra(integer(), non_neg_integer()) -> integer().
cutExtra(Offset, PointerWidth) ->
    Offset div PointerWidth * PointerWidth.

-spec primitiveSizeOf(atom()) -> pointerSize | 1 | 2 | 4 | 8.
primitiveSizeOf(usize) -> pointerSize;
primitiveSizeOf(isize) -> pointerSize;
primitiveSizeOf(u64) -> 8;
primitiveSizeOf(i64) -> 8;
primitiveSizeOf(u32) -> 4;
primitiveSizeOf(i32) -> 4;
primitiveSizeOf(u16) -> 2;
primitiveSizeOf(i16) -> 2;
primitiveSizeOf(u8) -> 1;
primitiveSizeOf(i8) -> 1;
primitiveSizeOf(f64) -> 8;
primitiveSizeOf(f32) -> 4;
primitiveSizeOf(T) -> throw(fmt("size of ~p is not defined", [T])).

voidType(Line) ->
    #basicType{class = void, tag = void, pdepth = 0, line = Line}.

-spec namesOfVariableReferences([#variableReference{}]) -> [atom()].
namesOfVariableReferences(VariableReferenceList) ->
    lists:map(fun (#variableReference{name = Name}) -> Name end, VariableReferenceList).

-spec namesOfVariableDefinitions([#variableDefinition{}]) -> [atom()].
namesOfVariableDefinitions(VariableDefinitionList) ->
    lists:map(fun (#variableDefinition{name = Name}) -> Name end, VariableDefinitionList).

-spec assert(boolean(), any()) -> ok.
assert(true, _) ->
    ok;
assert(false, Info) ->
    throw(Info).

-spec valueInList(any(), [any()]) -> boolean().
valueInList(Value, List) ->
    lists:any(fun (V) -> V =:= Value end, List).

%% filterVariableReferenceInMap([#variableReference{name = a}, #variableReference{name = b}], #{a => 1})
%% > [#variableReference{name = a}].
-spec filterVariableReferenceInMap([#variableReference{}], #{atom() := any()}) -> [#variableReference{}].
filterVariableReferenceInMap(VariableReferenceList, TargetMap) ->
    lists:filter(fun (#variableReference{name = Name}) -> existsInMap(Name, TargetMap) end, VariableReferenceList).

-ifdef(EUNIT).

filterVariableReferenceInMap_test() ->
    A = filterVariableReferenceInMap([#variableReference{name = a}, #variableReference{name = b}], #{a => 1}),
    ?assertEqual(A, [#variableReference{name = a}]).

-endif.

-spec existsInMap(atom(), #{atom() := any()}) -> boolean().
existsInMap(KeyName, Map) ->
    case maps:find(KeyName, Map) of
        {ok, _} ->
            true;
        _ ->
            false
    end.
