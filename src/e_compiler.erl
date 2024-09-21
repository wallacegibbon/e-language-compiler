-module(e_compiler).
-export([compile_to_ast/1, compile_to_machine1/2, compile_to_ir1/2, compile_to_c/2, compile_to_e/2]).
-export_type([e_compile_options/0]).
-include("e_record_definition.hrl").

-type token() :: {atom(), location(), _} | {atom(), location()}.
-type e_compile_options() :: map().

%% Compiling to C is supported in the early stage of this compiler. This function is archived and not used anymore.
-spec compile_to_c(string(), string()) -> ok.
compile_to_c(InputFilename, OutputFilename) ->
	try
		{AST, Vars, InitCode} = parse_and_compile(InputFilename, compiler_options()),
		e_dumper_c:generate_code(AST, Vars, InitCode, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

%% Compiling to E (with invalid syntax). This function is for debug, just like `compile_to_c/2`.
-spec compile_to_e(string(), string()) -> ok.
compile_to_e(InputFilename, OutputFilename) ->
	try
		{AST, _, InitCode} = parse_and_compile(InputFilename, compiler_options()),
		e_dumper_e:generate_code(AST, InitCode, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_ir1(string(), string()) -> ok.
compile_to_ir1(InputFilename, OutputFilename) ->
	Options = compiler_options(),
	#{wordsize := WordSize} = Options,
	try
		{AST, _, InitCode} = parse_and_compile(InputFilename, Options),
		e_dumper_ir1:generate_code(AST, InitCode, OutputFilename, WordSize)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_machine1(string(), string()) -> ok.
compile_to_machine1(InputFilename, OutputFilename) ->
	IR1Filename = OutputFilename ++ ".ir1",
	try
		compile_to_ir1(InputFilename, IR1Filename),
		{ok, IRs} = file:consult(IR1Filename),
		e_dumper_machine1:generate_code(IRs, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_ast(string()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_to_ast(Filename) ->
	try
		parse_and_compile(Filename, compiler_options())
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec parse_and_compile(string(), e_compile_options()) -> {e_ast_raw(), #e_vars{}, e_ast()}.
parse_and_compile(Filename, CompileOptions) ->
	e_ast_compiler:compile_from_raw_ast(parse_file(Filename), CompileOptions).

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

-spec compiler_options() -> e_compile_options().
compiler_options() ->
	maps:merge(default_compiler_options(), #{}).

-spec default_compiler_options() -> e_compile_options().
default_compiler_options() ->
	#{wordsize => 4}.

