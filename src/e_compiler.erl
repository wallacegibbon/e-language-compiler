-module(e_compiler).
-export([compile_to_ast/1, compile_to_machine1/2, compile_to_ir1/2, compile_to_c/2, compile_to_e/2]).
-include("e_record_definition.hrl").

-type token() :: {atom(), location(), _} | {atom(), location()}.

%% Compiling to C is supported in the early stage of this compiler. This function is archived and not used anymore.
-spec compile_to_c(string(), string()) -> ok.
compile_to_c(InputFilename, OutputFilename) ->
	try
		{AST, Vars, InitCode} = parse_and_compile(InputFilename, e_compile_option:default()),
		e_dumper_c:generate_code(AST, Vars, InitCode, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

%% Compiling to E (with invalid syntax). This function is for debug, just like `compile_to_c/2`.
-spec compile_to_e(string(), string()) -> ok.
compile_to_e(InputFilename, OutputFilename) ->
	try
		{AST, _, InitCode} = parse_and_compile(InputFilename, e_compile_option:default()),
		e_dumper_e:generate_code(AST, InitCode, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_ir1(string(), string()) -> #{non_neg_integer() => atom()}.
compile_to_ir1(InputFilename, OutputFilename) ->
	Options = e_compile_option:default(),
	#{ram_start_pos := StartPos, ram_end_pos := EndPos} = Options,
	try
		{AST, GlobalVars, InitCode} = parse_and_compile(InputFilename, Options),
		#e_vars{shifted_size = Size} = GlobalVars,
		e_dumper_ir1:generate_code(AST, InitCode, StartPos, EndPos - Size, OutputFilename, Options),
		fetch_interrupt_vector_table(AST)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

fetch_interrupt_vector_table(AST) ->
	Fns = lists:filter(fun(#e_function{interrupt = N}) when is_integer(N) -> true; (_) -> false end, AST),
	maps:from_list(lists:map(fun(#e_function{name = Name, interrupt = N}) -> {N, Name} end, Fns)).

-spec compile_to_machine1(string(), string()) -> ok.
compile_to_machine1(InputFilename, OutputFilename) ->
	IR1Filename = OutputFilename ++ ".ir1",
	try
		InterruptMap = compile_to_ir1(InputFilename, IR1Filename),
		{ok, IRs} = file:consult(IR1Filename),
		e_dumper_machine1:generate_code(IRs, OutputFilename, InterruptMap, e_compile_option:default())
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_ast(string()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_to_ast(Filename) ->
	try
		parse_and_compile(Filename, e_compile_option:default())
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec parse_and_compile(string(), e_compile_option:option()) -> {e_ast_raw(), #e_vars{}, e_ast()}.
parse_and_compile(Filename, CompileOptions) ->
	e_ast_compiler:compile_from_raw_ast(parse_file(Filename), CompileOptions).

-spec parse_file(string()) -> e_ast_raw().
parse_file(Filename) ->
	case file:read_file(Filename) of
		{ok, RawContent} ->
			parse_tokens(scan_raw_content(RawContent));
		{error, enoent} ->
			throw(e_util:fmt("file \"~s\"not found", [Filename]));
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

