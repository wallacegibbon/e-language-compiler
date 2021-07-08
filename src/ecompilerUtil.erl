-module(ecompilerUtil).

-export([expressionToString/1, expressionMap/2, filterVariableReferenceInMap/2, flatfmt/2, getValuesByKeys/2, namesOfVariableDefinitiions/1, namesOfVariableReferences/1, valueInList/2]).
-export([primitiveSizeOf/1, voidType/1]).
-export([cutExtra/2, fillOffset/2, fillToPointerWidth/2]).
-export([makeFunctionAndStructMapFromAST/1]).
-export([assert/2]).

-include_lib("eunit/include/eunit.hrl").
-include("./ecompilerFrameDef.hrl").

%% when do simple convertions, this function can be used to avoid boilerplate
%% code for if, while, return, call...,  so you can concentrate on op1, op2...
expressionMap(Fn, [#if_expr{condition = Cond, then = Then, else = Else} = If | Rest]) ->
    [If#if_expr{condition = Fn(Cond), then = expressionMap(Fn, Then), else = expressionMap(Fn, Else)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [#while_expr{condition = Cond, exprs = Exprs} = While | Rest]) ->
    [While#while_expr{condition = Fn(Cond), exprs = expressionMap(Fn, Exprs)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [#call{fn = Callee, args = Args} = Fncall | Rest]) ->
    [Fncall#call{fn = Fn(Callee), args = expressionMap(Fn, Args)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [#return{expr = Retexpr} = Return | Rest]) ->
    [Return#return{expr = Fn(Retexpr)} | expressionMap(Fn, Rest)];
expressionMap(Fn, [Any | Rest]) ->
    [Fn(Any) | expressionMap(Fn, Rest)];
expressionMap(_, []) ->
    [].

expressionToString(#if_expr{condition = Cond, then = Then, else = Else}) ->
    io_lib:format("if (~s) ~s else ~s end", [expressionToString(Cond), expressionToString(Then), expressionToString(Else)]);
expressionToString(#while_expr{condition = Cond, exprs = Exprs}) ->
    io_lib:format("while (~s) ~s end", [expressionToString(Cond), expressionToString(Exprs)]);
expressionToString(#call{fn = Callee, args = Args}) ->
    io_lib:format("(~s)(~s)", [expressionToString(Callee), expressionToString(Args)]);
expressionToString(#return{expr = Retexpr}) ->
    io_lib:format("return (~s)", [expressionToString(Retexpr)]);
expressionToString(#varref{name = Name}) ->
    io_lib:format("~s", [expressionToString(Name)]);
expressionToString(#op2{operator = Operator, op1 = Op1, op2 = Op2}) ->
    io_lib:format("~s ~s ~s", [expressionToString(Op1), Operator, expressionToString(Op2)]);
expressionToString(#op1{operator = Operator, operand = Operand}) ->
    io_lib:format("~s ~s", [expressionToString(Operand), Operator]);
expressionToString({Immi, _, Val}) when Immi =:= integer; Immi =:= float ->
    io_lib:format("~w", [Val]);
expressionToString({Immi, _, Val}) when Immi =:= string ->
    Val;
expressionToString(Any) ->
    Any.

flatfmt(FmtStr, Args) -> lists:flatten(io_lib:format(FmtStr, Args)).

-spec getValuesByKeys([atom()], #{atom() => any()}) -> [any()].
getValuesByKeys(Fields, Map) when is_map(Map) -> prvGetValuesByKeys(Fields, Map, []).

-spec prvGetValuesByKeys([atom()], #{atom() => any()}, [any()]) -> [any()].
prvGetValuesByKeys([Field | Rest], Map, Result) -> prvGetValuesByKeys(Rest, Map, [maps:get(Field, Map) | Result]);
prvGetValuesByKeys([], _, Result) -> lists:reverse(Result).

-ifdef(EUNIT).

getValuesByKeys_test() ->
    ?assertEqual([3, 2], getValuesByKeys([a, b], #{c => 1, b => 2, a => 3})).

-endif.

makeFunctionAndStructMapFromAST(AST) ->
    {Functions, Structs} = lists:partition(fun (A) -> element(1, A) =:= function end, AST),
    %% FnMap stores function type only
    FunctionMap = maps:from_list(lists:map(fun (#function{name = Name} = Fn) -> {Name, Fn#function.type} end, Functions)),
    StructMap = maps:from_list(lists:map(fun (#struct{name = Name} = S) -> {Name, S} end, Structs)),
    {FunctionMap, StructMap}.

%% address calculations
-spec fillToPointerWidth(integer(), non_neg_integer()) -> integer().
fillToPointerWidth(Num, PointerWidth) -> (Num + PointerWidth - 1) div PointerWidth * PointerWidth.

-spec fillOffset(integer(), non_neg_integer()) -> integer().
fillOffset(Offset, PointerWidth) -> (Offset + PointerWidth) div PointerWidth * PointerWidth.

-spec cutExtra(integer(), non_neg_integer()) -> integer().
cutExtra(Offset, PointerWidth) -> Offset div PointerWidth * PointerWidth.

-spec primitiveSizeOf(atom()) -> pwidth | 1 | 2 | 4 | 8.
primitiveSizeOf(usize) -> pwidth;
primitiveSizeOf(isize) -> pwidth;
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
primitiveSizeOf(T) -> throw(flatfmt("size of ~p is not defined", [T])).

voidType(Line) -> #basic_type{class = void, tag = void, pdepth = 0, line = Line}.

namesOfVariableReferences(VarRefs) -> lists:map(fun (#varref{name = N}) -> N end, VarRefs).

namesOfVariableDefinitiions(VarDefs) -> lists:map(fun (#vardef{name = N}) -> N end, VarDefs).

assert(false, Info) -> throw(Info);
assert(true, _) -> ok.

-spec valueInList(any(), [any()]) -> boolean().
valueInList(Value, List) -> lists:any(fun (V) -> V =:= Value end, List).

%% filter_varref_inmaps([#varref{name=a}, #varref{name=b}], #{a => 1})
%% > [#varref{name=a}].
filterVariableReferenceInMap(Varrefs, TargetMap) -> lists:filter(fun (#varref{name = N}) -> prvExistsInMap(N, TargetMap) end, Varrefs).

-ifdef(EUNIT).

filterVariableReferenceInMap_test() ->
    A = filterVariableReferenceInMap([#varref{name = a}, #varref{name = b}], #{a => 1}),
    ?assertEqual(A, [#varref{name = a}]).

-endif.

prvExistsInMap(Keyname, Map) ->
    case maps:find(Keyname, Map) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

