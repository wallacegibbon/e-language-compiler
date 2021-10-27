-module(ecompiler).

-export([compileToAST/1, compileToC/2, queryFunctionInModule/2]).

-compile({nowarn_unused_function, [{compilerRecordingDetails, 0}]}).

-include_lib("eunit/include/eunit.hrl").
-include("ecompilerFrameDef.hrl").

compileToC(InputFilename, OutputFilename) ->
    startCompilerRecordingProcess(filename:dirname(InputFilename)),
    try
        {AST, Vars, InitCode} = parseAndCompile(InputFilename),
        %io:format(">> ~p~n~n", [AST]),
        ecompilerGenerateCCode:generateCCode(AST, Vars, InitCode, OutputFilename)
    catch
        {Filename, Errinfo} ->
            io:format("~s: ~p~n", [Filename, Errinfo])
    end,
    stopCompilerRecordingProcess().

compileToAST(Filename) ->
    startCompilerRecordingProcess(filename:dirname(Filename)),
    R = parseAndCompile(Filename),
    stopCompilerRecordingProcess(),
    R.

parseAndCompile(Filename) ->
    ok = recordCompileFile(Filename),
    try
        {ok, AST} = parseFile(Filename),
        {Ast1, Vars, InitCode, FunctionTypeMap} = ecompilerCompile:compileFromRawAST(AST, #{}),
        ok = recordModule(Filename, FunctionTypeMap),
        {Ast1, Vars, InitCode}
    catch
        E ->
            ok = unRecordCompileFile(Filename),
            throw({Filename, E})
    end.

parseFile(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, RawContent} ->
            parseContent(RawContent);
        {error, enoent} ->
            throw("module not found");
        {error, Reason} ->
            throw(Reason)
    end.

parseContent(RawContent) ->
    case ecompilerScan:string(binary_to_list(RawContent)) of
        {ok, Tokens, _} ->
            case ecompilerParse:parse(Tokens) of
                {ok, _Ast} = D ->
                    D;
                {error, {Line, _, Errinfo}} ->
                    throw({Line, Errinfo})
            end;
        {error, Errors, Warnings} ->
            throw({Errors, Warnings})
    end.

%% start the recording process (record fnmap for module)
startCompilerRecordingProcess(SearchDir) ->
    State = #{searchdir => SearchDir, modmap => initialModuleASTMap(), moduleChain => []},
    case whereis(ecompilerHelper) of
        undefined ->
            Pid = spawn_link(fun () -> compilerRecordingLoop(State) end),
            register(ecompilerHelper, Pid);
        _ ->
            ok = changeSearchDirectory(SearchDir)
    end.

%% some c functions like printf, puts, malloc
initialModuleASTMap() ->
    CommonIntType = #basic_type{class = integer, tag = isize, pdepth = 0},
    CommonStrType = #basic_type{class = integer, tag = i8, pdepth = 1},
    #{c =>
        #{printf => #fun_type{params = [CommonStrType, CommonIntType], ret = CommonIntType},
          puts => #fun_type{params = [CommonStrType], ret = CommonIntType},
          malloc => #fun_type{params = [CommonIntType], ret = CommonStrType}}}.

stopCompilerRecordingProcess() ->
    try
        ok = compileRecordingCmd(stop),
        unregister(ecompilerHelper)
    catch
        error:_ ->
            ok
    end.

recordCompileFile(Filename) ->
    compileRecordingCmd({recordCompileOp, fileNameToModuleAtom(Filename)}).

unRecordCompileFile(Filename) ->
    compileRecordingCmd({unRecordCompileOp, fileNameToModuleAtom(Filename)}).

recordModule(Filename, FunctionTypeMap) ->
    compileRecordingCmd({recordModule, fileNameToModuleAtom(Filename), FunctionTypeMap}).

changeSearchDirectory(NewDir) ->
    compileRecordingCmd({changeSearchDirectory, NewDir}).

-spec fileNameToModuleAtom(string()) -> atom().
fileNameToModuleAtom(Filename) ->
    list_to_atom(filename:basename(Filename, ".e")).

queryFunctionInModule(ModName, FunName) ->
    case compileRecordingCmd({queryFunctionReturnType, ModName, FunName}) of
        {ok, _Type} = R ->
            R;
        {error, moduleNotFound, SearchDir} ->
            parseAndCompile(makeFileName(SearchDir, ModName)),
            queryFunctionInModule(ModName, FunName);
        {error, functionNotFound} = R ->
            R
    end.

makeFileName(SearchDir, ModName) ->
    lists:flatten(io_lib:format("~s/~s.e", [SearchDir, ModName])).

compilerRecordingDetails() ->
    compileRecordingCmd(debug).

compileRecordingCmd(Command) ->
    Ref = erlang:make_ref(),
    ecompilerHelper ! {{self(), Ref}, Command},
    receive
        {Ref, Result} ->
            Result
    end.

compilerRecordingLoop(State) ->
    receive
        {{Pid, Ref}, Command} ->
            case compileRecordingHandle(Command, State) of
                {reply, Result, NewState} ->
                    Pid ! {Ref, Result},
                    compilerRecordingLoop(NewState);
                stop ->
                    Pid ! {Ref, ok}
            end;
        Any ->
            throw(Any)
    end.

compileRecordingHandle({queryFunctionReturnType, ModName, FunName}, #{modmap := ModuleFnMap, searchdir := SearchDir} = State) ->
    case maps:find(ModName, ModuleFnMap) of
        {ok, FunctionTypeMap} ->
            case maps:find(FunName, FunctionTypeMap) of
                {ok, _Type} = D ->
                    {reply, D, State};
                error ->
                    {reply, {error, functionNotFound}, State}
            end;
        error ->
            {reply, {error, moduleNotFound, SearchDir}, State}
    end;
compileRecordingHandle({recordCompileOp, ModName}, #{moduleChain := ModuleChain} = State) ->
    NewModuleChain = [ModName | ModuleChain],
    case ecompilerUtil:valueInList(ModName, ModuleChain) of
        true ->
            {reply, {error, moduleRecursive, lists:reverse(NewModuleChain)}, State};
        false ->
            {reply, ok, State#{moduleChain := NewModuleChain}}
    end;
compileRecordingHandle({unRecordCompileOp, ModName}, #{moduleChain := [ModName | Rest]} = State) ->
    {reply, ok, State#{moduleChain := Rest}};
compileRecordingHandle({recordModule, ModName, FunctionTypeMap}, #{modmap := ModuleFnMap} = State) ->
    {reply, ok, State#{modmap := ModuleFnMap#{ModName => FunctionTypeMap}}};
compileRecordingHandle({changeSearchDirectory, NewDir}, State) ->
    {reply, ok, State#{searchdir := NewDir}};
compileRecordingHandle(debug, #{modmap := ModuleFnMap, moduleChain := ModuleChain} = State) ->
    {reply, {ModuleFnMap, ModuleChain}, State};
compileRecordingHandle(stop, _) ->
    stop.

-ifdef(EUNIT).

testStartBeforeTest(SearchDir) ->
    stopCompilerRecordingProcess(),
    startCompilerRecordingProcess(SearchDir),
    R = compilerRecordingDetails(),
    ?assertEqual(R, {initialModuleASTMap(), []}).

record_test() ->
    testStartBeforeTest("/tmp"),
    ok = recordCompileFile("./a/b/c/file1.e"),
    ok = recordCompileFile("./a/b/c/file2.e"),
    R3 = recordCompileFile("./a/b/c/file1.e"),
    ?assertEqual(R3, {error, moduleRecursive, [file1, file2, file1]}),
    ok.

getUncompiledModule_test() ->
    testStartBeforeTest("./sample"),
    R = queryFunctionInModule(simplemod, test),
    ?assertEqual(R, {ok, {fun_type, 3, [], {basic_type, 3, 0, integer, isize}}}),
    ok.

-endif.
