-module(ecompilerParseTest).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

functionNormal_test() ->
    {ok, Tokens, _} = ecompilerScan:string("fun a(v: i8^) v^ = 10; end"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{'fun', 1}, {identifier, 1, a}, {'(', 1}, {identifier, 1, v}, {':', 1}, {integerType, 1, i8}, {'^', 1}, {')', 1},
                  {identifier, 1, v}, {'^', 1}, {'=', 1}, {integer, 1, 10}, {';', 1}, {'end', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{functionRaw, 1, a, [{variableDefinition, 1, v, {basicType, 1, 1, integer, i8}, none}], {basicType, 1, 0, void, void},
                    [{operatorExpression2, 1, assign, {operatorExpression1, 1, '^', {variableReference, 1, v}}, {integer, 1, 10}}]}], AST),
    ok.

functionPointer_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: fun(u8, u8^): u16^ = b;"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'fun', 1}, {'(', 1}, {integerType, 1, u8}, {',', 1}, {integerType, 1, u8},
                  {'^', 1}, {')', 1}, {':', 1}, {integerType, 1, u16}, {'^', 1}, {'=', 1}, {identifier, 1, b}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variableDefinition, 1, a,
                   {functionType, 1, [{basicType, 1, 0, integer, u8}, {basicType, 1, 1, integer, u8}], {basicType, 1, 1, integer, u16}},
                   {variableReference, 1, b}}],
                 AST),
    ok.

functionCall_test() ->
    {ok, Tokens, _} = ecompilerScan:string("b: u8 = a(13);"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, b}, {':', 1}, {integerType, 1, u8}, {'=', 1}, {identifier, 1, a}, {'(', 1}, {integer, 1, 13}, {')', 1}, {';', 1}],
                 Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variableDefinition, 1, b, {basicType, 1, 0, integer, u8}, {call, 1, {variableReference, 1, a}, [{integer, 1, 13}]}}],
                 AST),
    ok.

array_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: {u8, 100};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {integerType, 1, u8}, {',', 1}, {integer, 1, 100}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variableDefinition, 1, a, {arrayType, 1, {basicType, 1, 0, integer, u8}, 100}, none}], AST),
    ok.

arrayInit_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: {u8, 2} = {11, 22};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {integerType, 1, u8}, {',', 1}, {integer, 1, 2}, {'}', 1},
                  {'=', 1}, {'{', 1}, {integer, 1, 11}, {',', 1}, {integer, 1, 22}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variableDefinition, 1, a, {arrayType, 1, {basicType, 1, 0, integer, u8}, 2},
                   {arrayInitializeExpression, 1, [{integer, 1, 11}, {integer, 1, 22}]}}], AST),
    ok.

structInit_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: S = S{name=\"a\", value=2};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {identifier, 1, 'S'}, {'=', 1}, {identifier, 1, 'S'}, {'{', 1}, {identifier, 1, name},
                  {'=', 1}, {string, 1, "a"}, {',', 1}, {identifier, 1, value}, {'=', 1}, {integer, 1, 2}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variableDefinition, 1, a, {basicType, 1, 0, struct, 'S'},
                   {structInitializeExpressionRaw, 1, 'S',
                    [{operatorExpression2, 1, assign, {variableReference, 1, name}, {string, 1, "a"}},
                     {operatorExpression2, 1, assign, {variableReference, 1, value}, {integer, 1, 2}}]}}],
                 AST),
    ok.

assign_test() ->
    {ok, Tokens, _} = ecompilerScan:string("fun b() a * = 3;c bsr = 5;end"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{'fun', 1}, {identifier, 1, b}, {'(', 1}, {')', 1}, {identifier, 1, a}, {'*', 1}, {'=', 1}, {integer, 1, 3},
                  {';', 1}, {identifier, 1, c}, {'bsr', 1}, {'=', 1}, {integer, 1, 5}, {';', 1}, {'end', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{functionRaw, 1, b, [], {basicType, 1, 0, void, void},
                    [{operatorExpression2, 1, assign, {variableReference, 1, a}, {operatorExpression2, 1, '*', {variableReference, 1, a}, {integer, 1, 3}}},
                     {operatorExpression2, 1, assign, {variableReference, 1, c}, {operatorExpression2, 1, 'bsr', {variableReference, 1, c}, {integer, 1, 5}}}]}],
                 AST),
    ok.

-endif.
