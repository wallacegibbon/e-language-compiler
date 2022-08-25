-module(ecompiler).
-export([compile_to_ast/1, compile_to_c/2]).

-include("e_record_definition.hrl").

-spec compile_to_c(string(), string()) -> ok.
compile_to_c(InputFilename, OutputFilename) ->
	try
		{AST, Vars, InitCode} = parse_and_compile(InputFilename),
		c_codegen:generate_c_code(AST, Vars, InitCode, OutputFilename)
	catch
		{Filename, ErrorInfo} ->
			io:format("~s: ~p~n", [Filename, ErrorInfo])
	end.

-spec compile_to_ast(string()) -> {e_ast(), var_type_map(), e_ast()}.
compile_to_ast(Filename) ->
	parse_and_compile(Filename).

-spec parse_and_compile(string()) -> {e_ast(), var_type_map(), e_ast()}.
parse_and_compile(Filename) ->
	try
		e_compiler:compile_from_raw_ast(parse_file(Filename), #{})
	catch
		E ->
			throw({Filename, E})
	end.

-spec parse_file(string()) -> e_ast().
parse_file(Filename) ->
	case file:read_file(Filename) of
		{ok, RawContent} ->
			parse_string(RawContent);
		{error, enoent} ->
			throw("file not found");
		{error, Reason} ->
			throw(Reason)
	end.

-spec parse_string(binary()) -> e_ast().
parse_string(RawContent) ->
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

