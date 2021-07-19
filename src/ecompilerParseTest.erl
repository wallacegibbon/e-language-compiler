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
    ?assertEqual([{function_raw, 1, a, [{vardef, 1, v, {basic_type, 1, 1, integer, i8}, none}], {basic_type, 1, 0, void, void},
                    [{op2, 1, assign, {op1, 1, '^', {varref, 1, v}}, {integer, 1, 10}}]}], AST),
    ok.

functionPointer_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: fun(u8, u8^): u16^ = b;"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'fun', 1}, {'(', 1}, {integerType, 1, u8}, {',', 1}, {integerType, 1, u8},
                    {'^', 1}, {')', 1}, {':', 1}, {integerType, 1, u16}, {'^', 1}, {'=', 1}, {identifier, 1, b}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{vardef, 1, a, {fun_type, 1, [{basic_type, 1, 0, integer, u8}, {basic_type, 1, 1, integer, u8}], {basic_type, 1, 1, integer, u16}}, {varref, 1, b}}], AST),
    ok.

functionCall_test() ->
    {ok, Tokens, _} = ecompilerScan:string("b: u8 = a::b(13);"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, b}, {':', 1}, {integerType, 1, u8}, {'=', 1}, {identifier, 1, a}, {'::', 1},
                    {identifier, 1, b}, {'(', 1}, {integer, 1, 13}, {')', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{vardef, 1, b, {basic_type, 1, 0, integer, u8}, {call, 1, {op2, 1, '::', {varref, 1, a}, {varref, 1, b}}, [{integer, 1, 13}]}}], AST),
    ok.

array_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: {u8, 100};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {integerType, 1, u8}, {',', 1}, {integer, 1, 100}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{vardef, 1, a, {array_type, 1, {basic_type, 1, 0, integer, u8}, {integer, 1, 100}}, none}], AST),
    ok.

arrayInit_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: {u8, 2} = {11, 22};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {integerType, 1, u8}, {',', 1}, {integer, 1, 2}, {'}', 1},
                    {'=', 1}, {'{', 1}, {integer, 1, 11}, {',', 1}, {integer, 1, 22}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{vardef, 1, a, {array_type, 1, {basic_type, 1, 0, integer, u8}, {integer, 1, 2}},
                    {array_init, 1, [{integer, 1, 11}, {integer, 1, 22}]}}], AST),
    ok.

structInit_test() ->
    {ok, Tokens, _} = ecompilerScan:string("a: S = S{name=\"a\", val=2};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {identifier, 1, 'S'}, {'=', 1}, {identifier, 1, 'S'}, {'{', 1}, {identifier, 1, name},
                    {'=', 1}, {string, 1, "a"}, {',', 1}, {identifier, 1, val}, {'=', 1}, {integer, 1, 2}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{vardef, 1, a, {basic_type, 1, 0, struct, 'S'},
                   {struct_init_raw, 1, 'S', [{op2, 1, assign, {varref, 1, name}, {string, 1, "a"}}, {op2, 1, assign, {varref, 1, val}, {integer, 1, 2}}]}}], AST),
    ok.

assign_test() ->
    {ok, Tokens, _} = ecompilerScan:string("fun b() a * = 3;c bsr = 5;end"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{'fun', 1}, {identifier, 1, b}, {'(', 1}, {')', 1}, {identifier, 1, a}, {'*', 1}, {'=', 1}, {integer, 1, 3},
                    {';', 1}, {identifier, 1, c}, {'bsr', 1}, {'=', 1}, {integer, 1, 5}, {';', 1}, {'end', 1}], Tokens),
    {ok, AST} = ecompilerParse:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{function_raw, 1, b, [], {basic_type, 1, 0, void, void},
                    [{op2, 1, assign, {varref, 1, a}, {op2, 1, '*', {varref, 1, a}, {integer, 1, 3}}},
                        {op2, 1, assign, {varref, 1, c}, {op2, 1, 'bsr', {varref, 1, c}, {integer, 1, 5}}}]}], AST),
    ok.

-endif.