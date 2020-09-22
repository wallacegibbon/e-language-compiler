-module(ecompiler).

-export([parse_and_compile/1, compile_to_c/2, query_modulefun/2]).

-import(ecompiler_utils, [flat_format/2, value_inlist/2]).

-compile({nowarn_unused_function, [record_details/0]}).

-include("./ecompiler_frame.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

compile_to_c(InputFilename, OutputFilename) ->
    try
	{Ast, Vars, InitCode} = parse_and_compile(InputFilename),
	%io:format(">> ~p~n~n", [Ast]),
	ecompiler_genc:generate_ccode(Ast, Vars, InitCode, OutputFilename),
	stop_compilercd()
    catch
	throw:{Filename, Errinfo} ->
	    io:format("~s: ~p~n", [Filename, Errinfo])
    end.

parse_and_compile(Filename) ->
    try
	%% start the compiling recording process (record fnmap for module)
	start_compilercd(filename:dirname(Filename)),
	%% record the file that will be compiled.
	record_compileop(Filename),
	{ok, Ast} = parse_file(Filename),
	Ret = ecompiler_compile:compile_from_rawast(Ast, #{}),
	%% Ret is {Ast, Vars, InitCode, FnMap}
	{Ast1, Vars, InitCode, FnMap} = Ret,
	record_module(Filename, FnMap),
	{Ast1, Vars, InitCode}
    catch
	throw:E ->
	    throw({Filename, E})
    end.

parse_file(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
	{ok, RawContent} ->
	    case ecompiler_scan:string(binary_to_list(RawContent)) of
		{ok, Tokens, _} ->
		    case ecompiler_parse:parse(Tokens) of
			{ok, _Ast} = D ->
			    D;
			{error, {Line, _, Errinfo}} ->
			    throw({Line, Errinfo})
		    end;
		{error, Errors, Warnings} ->
		    throw({Errors, Warnings})
	    end;
	{error, enoent} ->
	    throw("module not found");
	{error, Reason} ->
	    throw(Reason)
    end.

start_compilercd(SearchDir) ->
    State = #{searchdir => SearchDir, modmap => init_modmap(), modlinks => []},
    case whereis(ecompiler_helper) of
	undefined ->
	    Pid = spawn_link(fun() -> compilercd_loop(State) end),
	    register(ecompiler_helper, Pid);
	_ ->
	    already
    end.

%% some c functions like printf, puts, malloc
init_modmap() ->
    CommonIntType = #basic_type{class=integer, tag=isize, pdepth=0},
    CommonStrType = #basic_type{class=integer, tag=i8, pdepth=1},
    #{c => #{printf => #fun_type{params=[CommonStrType, CommonIntType],
				 ret=CommonIntType},
	     puts => #fun_type{params=[CommonStrType],
			       ret=CommonIntType},
	     malloc => #fun_type{params=[CommonIntType],
				 ret=CommonStrType}}}.

stop_compilercd() ->
    try
	ok = compilercd_cmd(stop),
	unregister(ecompiler_helper)
    catch
	error:_ ->
	    ok
    end.

record_compileop(Filename) ->
    compilercd_cmd({record_compileop, filename_tomod(Filename)}).

record_module(Filename, FnMap) ->
    ok = compilercd_cmd({record_module, filename_tomod(Filename), FnMap}).

filename_tomod(Filename) when is_list(Filename) ->
    list_to_atom(filename:basename(Filename, ".e")).

query_modulefun(ModName, FunName) ->
    case compilercd_cmd({query_funret, ModName, FunName}) of
	{error, module_notfound, SearchDir} ->
	    Filename = lists:flatten(io_lib:format("~s/~s.e",
						   [SearchDir, ModName])),
	    parse_and_compile(Filename),
	    query_modulefun(ModName, FunName);
	{error, function_notfound} = R ->
	    R;
	{ok, _Type} = R ->
	    R
    end.

record_details() ->
    compilercd_cmd(debug).

compilercd_cmd(Command) ->
    Ref = make_ref(),
    ecompiler_helper ! {self(), Ref, Command},
    receive
	{Ref, Result} ->
	    Result
    end.

compilercd_loop(State) ->
    receive
	{Pid, Ref, Command} ->
	    case compilercd_handle(Command, State) of
		{reply, Result, NewState} ->
		    Pid ! {Ref, Result},
		    compilercd_loop(NewState);
		stop ->
		    Pid ! {Ref, ok}
	    end;
	Any ->
	    throw(Any)
    end.

compilercd_handle({query_funret, ModName, FunName},
		  #{modmap := ModuleFnMap, searchdir := SearchDir} = State) ->
    case maps:find(ModName, ModuleFnMap) of
	{ok, FnMap} ->
	    case maps:find(FunName, FnMap) of
		{ok, _Type} = D ->
		    {reply, D, State};
		error ->
		    {reply, {error, function_notfound}, State}
	    end;
	error ->
	    {reply, {error, module_notfound, SearchDir}, State}
    end;
compilercd_handle({record_compileop, ModName},
		  #{modlinks := ModuleLinks} = State) ->
    NewModuleLinks = [ModName | ModuleLinks],
    case value_inlist(ModName, ModuleLinks) of
	true ->
	    {reply, {error, module_loop, lists:reverse(NewModuleLinks)},
	     State};
	_ ->
	    {reply, ok, State#{modlinks := NewModuleLinks}}
    end;
compilercd_handle({record_module, ModName, FnMap},
		  #{modmap := ModuleFnMap} = State) ->
    {reply, ok, State#{modmap := ModuleFnMap#{ModName => FnMap}}};
compilercd_handle(debug, #{modmap := ModuleFnMap,
			   modlinks := ModuleLinks} = State) ->
    {reply, {ModuleFnMap, ModuleLinks}, State};
compilercd_handle(stop, _) ->
    stop.

-ifdef(TEST).

start_beforetest(SearchDir) ->
    stop_compilercd(),
    start_compilercd(SearchDir),
    R = record_details(),
    ?assertEqual(R, {init_modmap(), []}).

record_test() ->
    start_beforetest("/tmp"),
    ok = record_compileop("./a/b/c/file1.e"),
    ok = record_compileop("./a/b/c/file2.e"),
    R3 = record_compileop("./a/b/c/file1.e"),
    ?assertEqual(R3, {error, module_loop, [file1, file2, file1]}),
    ok.

get_uncompiled_module_test() ->
    start_beforetest("./sample"),
    R = query_modulefun(simplemod, test),
    ?assertEqual(R, {ok, {fun_type,3,[],{basic_type,3,0,integer,isize}}}),
    ok.

-endif.

