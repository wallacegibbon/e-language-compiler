-module(e_dump_e).
-export([generate_e_code/3]).
-include("e_record_definition.hrl").

-spec generate_e_code(e_ast(), e_ast(), string()) -> ok.
generate_e_code(AST, InitCode, OutputFile) ->
	Code = statements_to_str(AST, InitCode),
	ok = file:write_file(OutputFile, Code).

-spec statements_to_str(e_ast(), [e_stmt()]) -> iolist().
statements_to_str([#e_function{name = main, stmts = Stmts} | Rest], InitCode) ->
	Body = string:join(lists:map(fun e_util:stmt_to_str/1, Stmts), "\n\t"),
	Init = string:join(lists:map(fun e_util:stmt_to_str/1, InitCode), "\n\t"),
	CodeStr = io_lib:format("fun ~s~n%%init~n\t~s~n%%init end~n~n\t~s~n~n", [main, Init, Body]),
	[CodeStr | statements_to_str(Rest, InitCode)];
statements_to_str([#e_function{name = Name, stmts = Stmts} | Rest], InitCode) ->
	Code = string:join(lists:map(fun e_util:stmt_to_str/1, Stmts), "\n\t"),
	CodeStr = io_lib:format("fun ~s~n\t~s~n~n", [Name, Code]),
	[CodeStr | statements_to_str(Rest, InitCode)];
statements_to_str([_ | Rest], InitCode) ->
	statements_to_str(Rest, InitCode);
statements_to_str([], _) ->
	[].

