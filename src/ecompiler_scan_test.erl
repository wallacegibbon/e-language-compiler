-module(ecompiler_scan_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

function_pointer_test() ->
    {ok, Tks, _} = ecompiler_scan:string("a: fun(u8, u8^): u16^ = b;"),
    %?debugFmt("~p~n", [Tks]),
    ?assertEqual(Tks, [{identifier,1,a}, {':',1}, {'fun',1}, {'(',1},
		       {basic_type,1,u8}, {',',1}, {basic_type,1,u8}, {'^',1},
		       {')',1}, {':',1}, {basic_type,1,u16}, {'^',1}, {'=',1},
		       {identifier,1,b}, {';',1}]),
    ok.

-endif.
