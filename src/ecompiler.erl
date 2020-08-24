-module(ecompiler).

-export([parse_file/1, parse_and_compile/1, compile_to_c/2]).

compile_to_c(InputFilename, OutputFilename) ->
    {ok, Ast} = parse_file(InputFilename),
    {Ast2, Vars, InitCode} = ecompiler_compile:compile_from_rawast(Ast, #{}),
    %io:format(">> ~p~n~n", [Ast2]),
    ecompiler_genc:generate_ccode(Ast2, Vars, InitCode, OutputFilename).


parse_file(Filename) when is_list(Filename) ->
    {ok, RawContent} = file:read_file(Filename),
    {ok, Tokens, _} = ecompiler_scan:string(binary_to_list(RawContent)),
    case ecompiler_parse:parse(Tokens) of
	{ok, Ast} ->
	    {ok, Ast};
	{error, E} ->
	    parse_error(E)
    end.

parse_and_compile(Filename) ->
    {ok, Ast} = parse_file(Filename),
    ecompiler_compile:compile_from_rawast(Ast, #{}).

parse_error({Line, _, Errinfo}) ->
    io:format("parse error(line ~w), ~s~n", [Line, Errinfo]).

