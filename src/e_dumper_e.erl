-module(e_dumper_e).
-export([generate_code/2]).
-include("e_record_definition.hrl").

-spec generate_code(e_ast_compiler:ast_compile_result(), string()) -> ok.
generate_code({{InitCode, AST}, _}, OutputFile) ->
    ok = file:write_file(OutputFile, ast_to_str(AST, InitCode)).

-spec ast_to_str(e_ast(), e_ast()) -> iolist().
ast_to_str([#e_function{name = main, stmts = Stmts} | Rest], InitCode) ->
    Body = string:join(lists:map(fun e_util:stmt_to_str/1, Stmts), "\n    "),
    Init = string:join(lists:map(fun e_util:stmt_to_str/1, InitCode), "\n    "),
    CodeStr = io_lib:format("fn ~s~n%%init~n    ~s~n%%init end~n~n    ~s~n~n", [main, Init, Body]),
    [CodeStr | ast_to_str(Rest, InitCode)];
ast_to_str([#e_function{name = Name, stmts = Stmts} | Rest], InitCode) ->
    Code = string:join(lists:map(fun e_util:stmt_to_str/1, Stmts), "\n    "),
    CodeStr = io_lib:format("fn ~s~n    ~s~n~n", [Name, Code]),
    [CodeStr | ast_to_str(Rest, InitCode)];
ast_to_str([_ | Rest], InitCode) ->
    ast_to_str(Rest, InitCode);
ast_to_str([], _) ->
    [].

