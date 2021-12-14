-module(e_parse_test).
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

function_normal_test() ->
    {ok, Tokens, _} = e_scanner:string("fun a(v: i8^) v^ = 10; end"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{'fun', 1}, {identifier, 1, a}, {'(', 1}, {identifier, 1, v}, {':', 1}, {integerType, 1, i8}, {'^', 1}, {')', 1},
                  {identifier, 1, v}, {'^', 1}, {'=', 1}, {integer, 1, 10}, {';', 1}, {'end', 1}], Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{function_raw, 1, a, [{variable_definition, 1, v, {basic_type, 1, 1, integer, i8}, none}], {basic_type, 1, 0, void, void},
                    [{operator_expression2, 1, assign, {operator_expression1, 1, '^', {variable_reference, 1, v}}, {integer, 1, 10}}]}], AST),
    ok.

function_pointer_test() ->
    {ok, Tokens, _} = e_scanner:string("a: fun(u8, u8^): u16^ = b;"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'fun', 1}, {'(', 1}, {integerType, 1, u8}, {',', 1}, {integerType, 1, u8},
                  {'^', 1}, {')', 1}, {':', 1}, {integerType, 1, u16}, {'^', 1}, {'=', 1}, {identifier, 1, b}, {';', 1}], Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variable_definition, 1, a,
                   {function_type, 1, [{basic_type, 1, 0, integer, u8}, {basic_type, 1, 1, integer, u8}], {basic_type, 1, 1, integer, u16}},
                   {variable_reference, 1, b}}],
                 AST),
    ok.

function_call_test() ->
    {ok, Tokens, _} = e_scanner:string("b: u8 = a(13);"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, b}, {':', 1}, {integerType, 1, u8}, {'=', 1}, {identifier, 1, a}, {'(', 1}, {integer, 1, 13}, {')', 1}, {';', 1}],
                 Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variable_definition, 1, b, {basic_type, 1, 0, integer, u8}, {call_expr, 1, {variable_reference, 1, a}, [{integer, 1, 13}]}}],
                 AST),
    ok.

array_test() ->
    {ok, Tokens, _} = e_scanner:string("a: {u8, 100};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {integerType, 1, u8}, {',', 1}, {integer, 1, 100}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variable_definition, 1, a, {array_type, 1, {basic_type, 1, 0, integer, u8}, 100}, none}], AST),
    ok.

array_init_test() ->
    {ok, Tokens, _} = e_scanner:string("a: {u8, 2} = {11, 22};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {integerType, 1, u8}, {',', 1}, {integer, 1, 2}, {'}', 1},
                  {'=', 1}, {'{', 1}, {integer, 1, 11}, {',', 1}, {integer, 1, 22}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variable_definition, 1, a, {array_type, 1, {basic_type, 1, 0, integer, u8}, 2},
                   {array_init_expr, 1, [{integer, 1, 11}, {integer, 1, 22}]}}], AST),
    ok.

struct_init_test() ->
    {ok, Tokens, _} = e_scanner:string("a: S = S{name=\"a\", value=2};"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{identifier, 1, a}, {':', 1}, {identifier, 1, 'S'}, {'=', 1}, {identifier, 1, 'S'}, {'{', 1}, {identifier, 1, name},
                  {'=', 1}, {string, 1, "a"}, {',', 1}, {identifier, 1, value}, {'=', 1}, {integer, 1, 2}, {'}', 1}, {';', 1}], Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{variable_definition, 1, a, {basic_type, 1, 0, struct, 'S'},
                   {struct_init_expr_raw, 1, 'S',
                    [{operator_expression2, 1, assign, {variable_reference, 1, name}, {string, 1, "a"}},
                     {operator_expression2, 1, assign, {variable_reference, 1, value}, {integer, 1, 2}}]}}],
                 AST),
    ok.

assign_test() ->
    {ok, Tokens, _} = e_scanner:string("fun b() a * = 3;c bsr = 5;end"),
    %?debugFmt("~p~n", [Tokens]),
    ?assertEqual([{'fun', 1}, {identifier, 1, b}, {'(', 1}, {')', 1}, {identifier, 1, a}, {'*', 1}, {'=', 1}, {integer, 1, 3},
                  {';', 1}, {identifier, 1, c}, {'bsr', 1}, {'=', 1}, {integer, 1, 5}, {';', 1}, {'end', 1}], Tokens),
    {ok, AST} = e_parser:parse(Tokens),
    %?debugFmt("~p~n", [AST]),
    ?assertEqual([{function_raw, 1, b, [], {basic_type, 1, 0, void, void},
                    [{operator_expression2, 1, assign, {variable_reference, 1, a}, {operator_expression2, 1, '*', {variable_reference, 1, a}, {integer, 1, 3}}},
                     {operator_expression2, 1, assign, {variable_reference, 1, c}, {operator_expression2, 1, 'bsr', {variable_reference, 1, c}, {integer, 1, 5}}}]}],
                 AST),
    ok.

-endif.
