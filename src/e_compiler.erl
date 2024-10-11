-module(e_compiler).
-export([main/1, compile_to_machine1/3, compile_to_ir1/3, compile_to_e/3, compile_to_c/3, compile_to_ast/2]).
-include("e_record_definition.hrl").

-type token() :: {atom(), location(), _} | {atom(), location()}.

%% Compiling to C is supported in the early stage of this compiler. This function is archived and not used anymore.
-spec compile_to_c(string(), string(), e_compile_option:option()) -> ok.
compile_to_c(InputFilename, OutputFilename, Options) ->
	#{wordsize := WordSize} = Options,
	try
		{Vars, AST, InitCode} = parse_and_compile(InputFilename, Options),
		e_dumper_c:generate_code(AST, InitCode, Vars, WordSize, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

%% Compiling to E (with invalid syntax). This function is for debug, just like `compile_to_c/2`.
-spec compile_to_e(string(), string(), e_compile_option:option()) -> ok.
compile_to_e(InputFilename, OutputFilename, Options) ->
	try
		{_, AST, InitCode} = parse_and_compile(InputFilename, Options),
		e_dumper_e:generate_code(AST, InitCode, OutputFilename)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_machine1(string(), string(), e_compile_option:option()) -> ok.
compile_to_machine1(InputFilename, OutputFilename, Options) ->
	IR1Filename = OutputFilename ++ ".ir1",
	try
		InterruptMap = compile_to_ir1(InputFilename, IR1Filename, Options),
		{ok, IRs} = file:consult(IR1Filename),
		e_dumper_machine1:generate_code(IRs, OutputFilename, InterruptMap, Options)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

-spec compile_to_ir1(string(), string(), e_compile_option:option()) -> #{non_neg_integer() => atom()}.
compile_to_ir1(InputFilename, OutputFilename, Options) ->
	#{data_pos := DataPos, data_size := DataSize} = Options,
	try
		{GlobalVars, AST, InitCode} = parse_and_compile(InputFilename, Options),
		#e_vars{shifted_size = Size} = GlobalVars,
		GP = DataPos + DataSize - Size,
		e_dumper_ir1:generate_code(AST, InitCode, DataPos, GP, OutputFilename, Options),
		fetch_interrupt_vector_table(AST)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [InputFilename, Line, Col, ErrorInfo]))
	end.

fetch_interrupt_vector_table(AST) ->
	Fns = lists:filter(fun(#e_function{interrupt = N}) when is_integer(N) -> true; (_) -> false end, AST),
	maps:from_list(lists:map(fun(#e_function{name = Name, interrupt = N}) -> {N, Name} end, Fns)).

-spec compile_to_ast(string(), e_compile_option:option()) -> {#e_vars{}, e_ast(), e_ast()}.
compile_to_ast(Filename, Options) ->
	try
		parse_and_compile(Filename, Options)
	catch
		{{Line, Col}, ErrorInfo} ->
			throw(e_util:fmt("~s:~w:~w: ~s~n", [Filename, Line, Col, ErrorInfo]))
	end.

-spec parse_and_compile(string(), e_compile_option:option()) -> {#e_vars{}, e_ast(), e_ast()}.
parse_and_compile(Filename, CompileOptions) ->
	e_ast_compiler:compile_from_raw_ast(parse_file(Filename), CompileOptions).

-spec parse_file(string()) -> e_ast_raw().
parse_file(Filename) ->
	Tokens = scan_file(Filename),
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
			scan_raw_content(RawContent);
		{error, enoent} ->
			throw(e_util:fmt("file ~s is not found", [Filename]));
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

%% for escript generating
main(_) ->
	io:format("call ec:main/1 instead~n").

