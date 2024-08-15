-module(e_parse_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

function_normal_test() ->
	{ok, Tokens, _} = e_scanner:string("fn a(v: i8^) v^ = 10; end"),
	?assertEqual([{'fn', 1}, {identifier, 1, a}, {'(', 1}, {identifier, 1, v}, {':', 1}, {int_type, 1, i8},
			{'^', 1}, {')', 1}, {identifier, 1, v}, {'^', 1}, {'=', 1}, {integer, 1, 10},
			{';', 1}, {'end', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_function_raw, 1, a, [{e_vardef, 1, v, {e_basic_type, 1, 1, integer, i8}, none}],
			{e_basic_type, 1, 0, void, void},
			[{e_op, 1, '=', [{e_op, 1, '^', [{e_varref, 1, v}]}, {e_integer, 1, 10}]}]}], AST),
	ok.

function_pointer_test() ->
	{ok, Tokens, _} = e_scanner:string("a: fn (u8, u8^): u16^ = b;"),
	?assertEqual([{identifier, 1, a}, {':', 1}, {'fn', 1}, {'(', 1}, {int_type, 1, u8}, {',', 1},
			{int_type, 1, u8}, {'^', 1}, {')', 1}, {':', 1}, {int_type, 1, u16}, {'^', 1},
			{'=', 1}, {identifier, 1, b}, {';', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_vardef, 1, a, {e_fn_type, 1, [{e_basic_type, 1, 0, integer, u8}, {e_basic_type, 1, 1, integer, u8}],
			{e_basic_type, 1, 1, integer, u16}}, {e_varref, 1, b}}], AST),
	ok.

function_call_test() ->
	{ok, Tokens, _} = e_scanner:string("b: u8 = a(13);"),
	?assertEqual([{identifier, 1, b}, {':', 1}, {int_type, 1, u8}, {'=', 1}, {identifier, 1, a},
			{'(', 1}, {integer, 1, 13}, {')', 1}, {';', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_vardef, 1, b, {e_basic_type, 1, 0, integer, u8},
			{e_op, 1, {call, {e_varref, 1, a}}, [{e_integer, 1, 13}]}}], AST),
	ok.

array_test() ->
	{ok, Tokens, _} = e_scanner:string("a: {u8, 100};"),
	?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {int_type, 1, u8}, {',', 1}, {integer, 1, 100},
			{'}', 1}, {';', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_vardef, 1, a, {e_array_type, 1, {e_basic_type, 1, 0, integer, u8}, 100}, none}], AST),
	ok.

array_init_test() ->
	{ok, Tokens, _} = e_scanner:string("a: {u8, 2} = {11, 22};"),
	?assertEqual([{identifier, 1, a}, {':', 1}, {'{', 1}, {int_type, 1, u8}, {',', 1}, {integer, 1, 2},
			{'}', 1}, {'=', 1}, {'{', 1}, {integer, 1, 11}, {',', 1},
			{integer, 1, 22}, {'}', 1}, {';', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_vardef, 1, a, {e_array_type, 1, {e_basic_type, 1, 0, integer, u8}, 2},
			{e_array_init_expr, 1, [{e_integer, 1, 11}, {e_integer, 1, 22}]}}], AST),
	ok.

struct_init_test() ->
	{ok, Tokens, _} = e_scanner:string("a: S = S{name=\"a\", value=2};"),
	?assertEqual([{identifier, 1, a}, {':', 1}, {identifier, 1, 'S'}, {'=', 1}, {identifier, 1, 'S'},
			{'{', 1}, {identifier, 1, name}, {'=', 1}, {string, 1, "a"}, {',', 1},
			{identifier, 1, value}, {'=', 1}, {integer, 1, 2}, {'}', 1}, {';', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_vardef, 1, a, {e_basic_type, 1, 0, struct, 'S'},
			{e_struct_init_raw_expr, 1, 'S',
			 [{e_op, 1, '=', [{e_varref, 1, name}, {e_string, 1, "a"}]},
				{e_op, 1, '=', [{e_varref, 1, value}, {e_integer, 1, 2}]}]}}], AST),
	ok.

assign_test() ->
	{ok, Tokens, _} = e_scanner:string("fn b() a * = 3; c bsr = 5; end"),
	?assertEqual([{'fn', 1}, {identifier, 1, b}, {'(', 1}, {')', 1}, {identifier, 1, a}, {'*', 1},
			{'=', 1}, {integer, 1, 3}, {';', 1}, {identifier, 1, c}, {'bsr', 1}, {'=', 1},
			{integer, 1, 5}, {';', 1}, {'end', 1}], Tokens),
	{ok, AST} = e_parser:parse(Tokens),
	?assertEqual([{e_function_raw, 1, b, [], {e_basic_type, 1, 0, void, void},
		       [{e_op, 1, '=', [{e_varref, 1, a}, {e_op, 1, '*', [{e_varref, 1, a}, {e_integer, 1, 3}]}]},
			{e_op, 1, '=', [{e_varref, 1, c}, {e_op, 1, 'bsr', [{e_varref, 1, c}, {e_integer, 1, 5}]}]}]}], AST),
	ok.

-endif.
