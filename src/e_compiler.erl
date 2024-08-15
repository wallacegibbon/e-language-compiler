-module(e_compiler).
-export([compile_to_ast/1, compile_to_c/2, compile_to_e/2]).
-include("e_record_definition.hrl").

-type token() :: {atom(), location(), _} | {atom(), location()}.

-spec compile_to_e(string(), string()) -> ok.
compile_to_e(InputFilename, OutputFilename) ->
	try
		{AST, _, InitCode} = parse_and_compile(InputFilename),
		e_dump_e:generate_e_code(AST, InitCode, OutputFilename)
	catch
		{Filename, {{Line, Col}, ErrorInfo}} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_c(string(), string()) -> ok.
compile_to_c(InputFilename, OutputFilename) ->
	try
		{AST, Vars, InitCode} = parse_and_compile(InputFilename),
		e_dump_c:generate_c_code(AST, Vars, InitCode, OutputFilename)
	catch
		{Filename, {{Line, Col}, ErrorInfo}} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_ast(string()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_to_ast(Filename) ->
	try
		parse_and_compile(Filename)
	catch
		{Filename, {{Line, Col}, ErrorInfo}} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec parse_and_compile(string()) -> {e_ast_raw(), #e_vars{}, e_ast()}.
parse_and_compile(Filename) ->
	try
		e_ast_compiler:compile_from_raw_ast(parse_file(Filename), #{})
	catch
		E ->
			throw({Filename, E})
	end.

-spec parse_file(string()) -> e_ast_raw().
parse_file(Filename) ->
	case file:read_file(Filename) of
		{ok, RawContent} ->
			parse_tokens(scan_raw_content(RawContent));
		{error, enoent} ->
			throw(io_lib:format("file \"~s\"not found", [Filename]));
		{error, Reason} ->
			throw(Reason)
	end.

-spec scan_raw_content(binary()) -> [token()].
scan_raw_content(RawContent) ->
	case e_scanner:string(unicode:characters_to_list(RawContent, utf8)) of
		{ok, Tokens, _EndLoc} ->
			Tokens;
		{error, {_, _, {user, {Loc, Info}}}, _} ->
			throw({Loc, Info});
		Error->
			throw(Error)
	end.

-spec parse_tokens([token()]) -> e_ast_raw().
parse_tokens(Tokens) ->
	case e_parser:parse(e_preprocessor:process(Tokens)) of
		{ok, AST} ->
			AST;
		{error, {Loc, _, ErrorInfo}} ->
			throw({Loc, ErrorInfo})
	end.

