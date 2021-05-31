-module(ecompiler).

-export([compile_to_ast/1, compile_to_c/2, query_modulefun/2]).

-compile({nowarn_unused_function, [{record_details, 0}]}).

-include("./ecompiler_frame.hrl").

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

-endif.

compile_to_c(InputFilename, OutputFilename) ->
    start_compilercd(filename:dirname(InputFilename)),
    try
        {Ast, Vars, InitCode} = parse_and_compile(InputFilename),
        %io:format(">> ~p~n~n", [Ast]),
        ecompiler_genc:generate_ccode(Ast, Vars, InitCode, OutputFilename)
    catch
        {Filename, Errinfo} ->
            io:format("~s: ~p~n", [Filename, Errinfo])
    end,
    stop_compilercd().

compile_to_ast(Filename) ->
    start_compilercd(filename:dirname(Filename)),
    R = parse_and_compile(Filename),
    stop_compilercd(),
    R.

parse_and_compile(Filename) ->
    ok = record_compileop(Filename),
    try
        {ok, Ast} = parse_file(Filename),
        Ret = ecompiler_compile:compile_from_rawast(Ast, #{}),
        {Ast1, Vars, InitCode, FnMap} = Ret,
        ok = record_module(Filename, FnMap),
        {Ast1, Vars, InitCode}
    catch
        E ->
            ok = unrecord_compileop(Filename),
            throw({Filename, E})
    end.

parse_file(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, RawContent} ->
            parse_content(RawContent);
        {error, enoent} ->
            throw("module not found");
        {error, Reason} ->
            throw(Reason)
    end.

parse_content(RawContent) ->
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
    end.

%% start the recording process (record fnmap for module)
start_compilercd(SearchDir) ->
    State = #{searchdir => SearchDir, modmap => init_modmap(), modchain => []},
    case whereis(ecompiler_helper) of
        undefined ->
            Pid = spawn_link(fun () -> compilercd_loop(State) end),
            register(ecompiler_helper, Pid);
        _ ->
            ok = change_searchdir(SearchDir)
    end.

%% some c functions like printf, puts, malloc
init_modmap() ->
    CommonIntType = #basic_type{class = integer, tag = isize, pdepth = 0},
    CommonStrType = #basic_type{class = integer, tag = i8, pdepth = 1},
    #{c =>
        #{printf =>
            #fun_type{params = [CommonStrType, CommonIntType],
                      ret = CommonIntType},
          puts =>
            #fun_type{params = [CommonStrType],
                      ret = CommonIntType},
          malloc =>
            #fun_type{params = [CommonIntType],
                      ret = CommonStrType}}}.

stop_compilercd() ->
    try
        ok = compilercd_cmd(stop),
        unregister(ecompiler_helper)
    catch
        error:_ -> ok
    end.

record_compileop(Filename) ->
    compilercd_cmd({record_compileop, filename_tomod(Filename)}).

unrecord_compileop(Filename) ->
    compilercd_cmd({unrecord_compileop, filename_tomod(Filename)}).

record_module(Filename, FnMap) ->
    compilercd_cmd({record_module, filename_tomod(Filename), FnMap}).

change_searchdir(NewDir) ->
    compilercd_cmd({change_searchdir, NewDir}).

filename_tomod(Filename) when is_list(Filename) ->
    list_to_atom(filename:basename(Filename, ".e")).

query_modulefun(ModName, FunName) ->
    case compilercd_cmd({query_funret, ModName, FunName}) of
        {error, module_notfound, SearchDir} ->
            parse_and_compile(mk_filename(SearchDir, ModName)),
            query_modulefun(ModName, FunName);
        {error, function_notfound} = R ->
            R;
        {ok, _Type} = R ->
            R
    end.

mk_filename(SearchDir, ModName) ->
    lists:flatten(io_lib:format("~s/~s.e", [SearchDir, ModName])).

record_details() ->
    compilercd_cmd(debug).

compilercd_cmd(Command) ->
    Ref = make_ref(),
    ecompiler_helper ! {{self(), Ref}, Command},
    receive {Ref, Result} -> Result end.

compilercd_loop(State) ->
    receive
        {{Pid, Ref}, Command} ->
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
                  #{modchain := ModuleChain} = State) ->
    NewModuleChain = [ModName | ModuleChain],
    case ecompiler_util:value_inlist(ModName, ModuleChain) of
        true ->
            {reply, {error, module_loop, lists:reverse(NewModuleChain)},
             State};
        _ ->
            {reply, ok, State#{modchain := NewModuleChain}}
    end;
compilercd_handle({unrecord_compileop, ModName},
                  #{modchain := [ModName | Rest]} = State) ->
    {reply, ok, State#{modchain := Rest}};
compilercd_handle({record_module, ModName, FnMap},
                  #{modmap := ModuleFnMap} = State) ->
    {reply, ok, State#{modmap := ModuleFnMap#{ModName => FnMap}}};
compilercd_handle({change_searchdir, NewDir}, State) ->
    {reply, ok, State#{searchdir := NewDir}};
compilercd_handle(debug, #{modmap := ModuleFnMap,
                           modchain := ModuleChain} = State) ->
    {reply, {ModuleFnMap, ModuleChain}, State};
compilercd_handle(stop, _) ->
    stop.

-ifdef(EUNIT).

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
    ?assertEqual(R3,
                 {error, module_loop, [file1, file2, file1]}),
    ok.

get_uncompiled_module_test() ->
    start_beforetest("./sample"),
    R = query_modulefun(simplemod, test),
    ?assertEqual(R,
                 {ok,
                  {fun_type, 3, [], {basic_type, 3, 0, integer, isize}}}),
    ok.

-endif.

