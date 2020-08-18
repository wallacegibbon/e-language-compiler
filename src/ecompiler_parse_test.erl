-module(ecompiler_parse_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

function_normal_test() ->
    {ok, Tks, _} = ecompiler_scan:string( "fun blah(val: i8^): void val^ = 10; end"),
    {ok, Ast} = ecompiler_parse:parse(Tks),
    %?debugFmt("~p~n", [Ast]),
    ?assertEqual(Ast, [{function_raw,1,blah,
			[{vardef,1,val,{basic_type,1,{i8,1}},none}],
			{basic_type,1,{void,0}},
			[{op2,1,assign,
			  {op1,1,'^',{varref,1,val}},
			  {integer,1,10}}]}]),
    ok.

function_pointer_test() ->
    {ok, Tks, _} = ecompiler_scan:string("a: fun(u8, u8^): u16^ = b;"),
    %?debugFmt("~p~n", [Tks]),
    {ok, Ast} = ecompiler_parse:parse(Tks),
    %?debugFmt("~p~n", [Ast]),
    ?assertEqual(Ast, [{vardef,1,a,
			{fun_type,1, [{basic_type,1,{u8,0}},
				      {basic_type,1,{u8,1}}],
			 {basic_type,1,{u16,1}}},
			{varref,1,b}}]),
    ok.

-endif.
