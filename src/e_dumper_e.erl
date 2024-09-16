-module(e_dumper_e).
-export([generate_code/3]).
-include("e_record_definition.hrl").

-spec generate_code(e_ast(), e_ast(), string()) -> ok.
generate_code(AST, InitCode, OutputFile) ->
	ok = file:write_file(OutputFile, ast_to_str(AST, InitCode)).

-spec ast_to_str(e_ast(), [e_stmt()]) -> iolist().
ast_to_str([#e_function{name = main, stmts = Stmts} | Rest], InitCode) ->
	Stmts1 = e_util:expr_map(fun fix_fn_varref/1, Stmts),
	Body = string:join(lists:map(fun e_util:stmt_to_str/1, Stmts1), "\n\t"),
	InitCode1 = e_util:expr_map(fun fix_fn_varref/1, InitCode),
	Init = string:join(lists:map(fun e_util:stmt_to_str/1, InitCode1), "\n\t"),
	CodeStr = io_lib:format("fn ~s~n%%init~n\t~s~n%%init end~n~n\t~s~n~n", [main, Init, Body]),
	[CodeStr | ast_to_str(Rest, InitCode)];
ast_to_str([#e_function{name = Name, stmts = Stmts} | Rest], InitCode) ->
	Stmts1 = e_util:expr_map(fun fix_fn_varref/1, Stmts),
	Code = string:join(lists:map(fun e_util:stmt_to_str/1, Stmts1), "\n\t"),
	CodeStr = io_lib:format("fn ~s~n\t~s~n~n", [Name, Code]),
	[CodeStr | ast_to_str(Rest, InitCode)];
ast_to_str([_ | Rest], InitCode) ->
	ast_to_str(Rest, InitCode);
ast_to_str([], _) ->
	[].

fix_fn_varref(#e_op{tag = {call, Callee}, data = Args} = Op) ->
	Op#e_op{tag = {call, fix_fn_varref(Callee)}, data = lists:map(fun fix_fn_varref/1, Args)};
fix_fn_varref(#e_op{data = Args} = Op) ->
	Op#e_op{data = lists:map(fun fix_fn_varref/1, Args)};
fix_fn_varref(#e_varref{name = {fn, Name}} = Varref) ->
	Varref#e_varref{name = list_to_atom("fn_" ++ atom_to_list(Name))};
fix_fn_varref(Any) ->
	Any.

