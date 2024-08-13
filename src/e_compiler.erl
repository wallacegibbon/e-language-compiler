-module(e_compiler).
-export([compile_to_ast/1, compile_to_c/2, compile_to_e/2]).
-include("e_record_definition.hrl").

-spec compile_to_e(string(), string()) -> ok.
compile_to_e(InputFilename, OutputFilename) ->
	try
		{AST, Vars, InitCode} = parse_and_compile(InputFilename),
		e_dump_e:generate_e_code(AST, InitCode, OutputFilename)
	catch
		{Filename, {Line, ErrorInfo}} ->
			throw(e_util:fmt("~s:~w: ~s~n", [Filename, Line, ErrorInfo]))
	end.

-spec compile_to_c(string(), string()) -> ok.
compile_to_c(InputFilename, OutputFilename) ->
	try
		{AST, Vars, InitCode} = parse_and_compile(InputFilename),
		e_dump_c:generate_c_code(AST, Vars, InitCode, OutputFilename)
	catch
		{Filename, {Line, ErrorInfo}} ->
			throw(e_util:fmt("~s:~w: ~s~n", [Filename, Line, ErrorInfo]))
	end.

-spec compile_to_ast(string()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_to_ast(Filename) ->
	try
		parse_and_compile(Filename)
	catch
		{Filename, {Line, ErrorInfo}} ->
			throw(e_util:fmt("~s:~w: ~s~n", [Filename, Line, ErrorInfo]))
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
			parse_content(RawContent);
		{error, enoent} ->
			throw(io_lib:format("file \"~s\"not found", [Filename]));
		{error, Reason} ->
			throw(Reason)
	end.

-spec parse_content(binary()) -> e_ast_raw().
parse_content(RawContent) ->
	case e_scanner:string(binary_to_list(RawContent)) of
		{ok, Tokens, _} ->
			case e_parser:parse(e_preprocessor:process(Tokens)) of
				{ok, AST} ->
					AST;
				{error, {Line, _, ErrorInfo}} ->
					throw({Line, ErrorInfo})
			end;
		{error, ErrorInfo, Line} ->
			throw({Line, ErrorInfo})
	end.

