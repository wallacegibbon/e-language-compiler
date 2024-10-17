-module(e_compiler).
-export([main/1, compile_to_ir1/3, compile_to_machine1/3, compile_to_e/3, compile_to_c/3, parse_and_compile_files/2]).
-compile([{nowarn_unused_function, [{parse_and_compile, 2}]}]).
-include("e_record_definition.hrl").

-spec compile_to_ir1([string()], string(), e_compile_option:option()) -> ok.
compile_to_ir1(InputFiles, OutputFilename, Options) when is_list(hd(InputFiles)) ->
	try
		e_dumper_ir1:generate_code(parse_and_compile_files(InputFiles, Options), OutputFilename, Options)
	catch
		{{Filename, Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_machine1([string()], string(), e_compile_option:option()) -> ok.
compile_to_machine1(InputFiles, OutputFilename, Options) when is_list(hd(InputFiles)) ->
	compile_to_ir1(InputFiles, OutputFilename, Options),
	{ok, CodeIRs} = file:consult(OutputFilename ++ ".code.ir1"),
	{ok, IVecIRs} = file:consult(OutputFilename ++ ".ivec.ir1"),
	e_dumper_machine1:generate_code(CodeIRs, IVecIRs, OutputFilename, Options).

%% Compiling to C is supported in the early stage of this compiler. This function is archived and not used anymore.
-spec compile_to_c([string()], string(), e_compile_option:option()) -> ok.
compile_to_c(InputFiles, OutputFilename, #{wordsize := WordSize} = Options) ->
	try
		e_dumper_c:generate_code(parse_and_compile_files(InputFiles, Options), WordSize, OutputFilename)
	catch
		{{Filename, Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

%% Compiling to E (with invalid syntax). This function is for debug, just like `compile_to_c/2`.
-spec compile_to_e([string()], string(), e_compile_option:option()) -> ok.
compile_to_e(InputFiles, OutputFilename, Options) ->
	try
		e_dumper_e:generate_code(parse_and_compile_files(InputFiles, Options), OutputFilename)
	catch
		{{Filename, Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec parse_and_compile_files([string()], e_compile_option:option()) -> e_ast_compiler:ast_compile_result().
parse_and_compile_files(Files, Options) ->
	Tokens0 = lists:concat(lists:map(fun(Filename) -> scan_file(Filename) end, Files)),
	Tokens1 = e_preprocessor:preprocess(Tokens0),
	e_ast_compiler:compile_from_raw_ast(parse_tokens(Tokens1), Options).

-spec parse_and_compile(string(), e_compile_option:option()) -> e_ast_compiler:ast_compile_result().
parse_and_compile(Filename, Options) ->
	Tokens0 = scan_file(Filename),
	Tokens1 = e_preprocessor:preprocess(Tokens0),
	e_ast_compiler:compile_from_raw_ast(parse_tokens(Tokens1), Options).

-spec parse_tokens([token()]) -> e_ast_raw().
parse_tokens(Tokens) ->
	case e_parser:parse(Tokens) of
		{ok, AST} ->
			AST;
		{error, {Loc, _, ErrorInfo}} ->
			throw({Loc, ErrorInfo})
	end.

-spec scan_file(string()) -> [token()].
scan_file(Filename) ->
	case file:read_file(Filename) of
		{ok, RawContent} ->
			RawTokens = scan_raw_content(RawContent),
			lists:map(fun(T) -> e_util:token_attach_filename(Filename, T) end, RawTokens);
		{error, enoent} ->
			throw(e_util:fmt("file ~s is not found", [Filename]));
		{error, Reason} ->
			throw(Reason)
	end.

-spec scan_raw_content(binary()) -> [raw_token()].
scan_raw_content(RawContent) ->
	case e_scanner:string(unicode:characters_to_list(RawContent, utf8)) of
		{ok, Tokens, _EndLoc} ->
			Tokens;
		{error, {_, _, {user, {Loc, Info}}}, _} ->
			throw({Loc, Info});
		Error->
			throw(Error)
	end.

%% for escript generating
main(_) ->
	io:format("call ec:main/1 instead~n").

