-module(ecompiler).

-export([parse_file/1]).

parse_file(Filename) when is_list(Filename) ->
    {ok, RawContent} = file:read_file(Filename),
    {ok, Tokens, _} = ecompiler_scan:string(binary_to_list(RawContent)),
    case ecompiler_parse:parse(Tokens) of
	{ok, Ast} ->
	    {ok, Ast};
	{error, E} ->
	    parse_error(E)
    end.


parse_error({Line, _, Errinfo}) ->
    io:format("parse error(line ~w), ~s~n", [Line, Errinfo]).

