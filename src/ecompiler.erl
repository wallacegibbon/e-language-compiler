-module(ecompiler).

-export([compileToAST/1, compileToC/2, prvQueryFunctionInModule/2]).

-compile({nowarn_unused_function, [{prvCompilerRecordingDetails, 0}]}).

-include_lib("eunit/include/eunit.hrl").
-include("./ecompilerFrameDef.hrl").

compileToC(InputFilename, OutputFilename) ->
    prvStartCompilerRecordingProcess(filename:dirname(InputFilename)),
    try
        {AST, Vars, InitCode} = prvParseAndCompile(InputFilename),
        %io:format(">> ~p~n~n", [AST]),
        ecompilerGenerateCCode:generateCCode(AST, Vars, InitCode, OutputFilename)
    catch
        {Filename, Errinfo} ->
            io:format("~s: ~p~n", [Filename, Errinfo])
    end,
    prvStopCompilerRecordingProcess().

compileToAST(Filename) ->
    prvStartCompilerRecordingProcess(filename:dirname(Filename)),
    R = prvParseAndCompile(Filename),
    prvStopCompilerRecordingProcess(),
    R.

prvParseAndCompile(Filename) ->
    ok = prvRecordCompileFile(Filename),
    try
        {ok, AST} = prvParseFile(Filename),
        Ret = ecompilerCompile:compileFromRawAST(AST, #{}),
        {Ast1, Vars, InitCode, FunctionTypeMap} = Ret,
        ok = recordModule(Filename, FunctionTypeMap),
        {Ast1, Vars, InitCode}
    catch
        E ->
            ok = prvUnRecordCompileFile(Filename),
            throw({Filename, E})
    end.

prvParseFile(Filename) when is_list(Filename) ->
    case file:read_file(Filename) of
        {ok, RawContent} ->     prvParseContent(RawContent);
        {error, enoent} ->      throw("module not found");
        {error, Reason} ->      throw(Reason)
    end.

prvParseContent(RawContent) ->
    case ecompilerScan:string(binary_to_list(RawContent)) of
        {ok, Tokens, _} ->
            case ecompilerParse:parse(Tokens) of
                {ok, _Ast} = D ->                   D;
                {error, {Line, _, Errinfo}} ->      throw({Line, Errinfo})
            end;
        {error, Errors, Warnings} ->
            throw({Errors, Warnings})
    end.

%% start the recording process (record fnmap for module)
prvStartCompilerRecordingProcess(SearchDir) ->
    State = #{searchdir => SearchDir, modmap => prvInitialModuleASTMap(), moduleChain => []},
    case whereis(ecompilerHelper) of
        undefined ->
            Pid = spawn_link(fun () -> prvCompilerRecordingLoop(State) end),
            register(ecompilerHelper, Pid);
        _ ->
            ok = prvChangeSearchDirectory(SearchDir)
    end.

%% some c functions like printf, puts, malloc
prvInitialModuleASTMap() ->
    CommonIntType = #basic_type{class = integer, tag = isize,   pdepth = 0},
    CommonStrType = #basic_type{class = integer, tag = i8,      pdepth = 1},
    #{c =>
        #{printf =>     #fun_type{params = [CommonStrType, CommonIntType],  ret = CommonIntType},
          puts =>       #fun_type{params = [CommonStrType],                 ret = CommonIntType},
          malloc =>     #fun_type{params = [CommonIntType],                 ret = CommonStrType}}}.

prvStopCompilerRecordingProcess() ->
    try
        ok = prvCompileRecordingCmd(stop),
        unregister(ecompilerHelper)
    catch
        error:_ -> ok
    end.

prvRecordCompileFile(Filename) -> prvCompileRecordingCmd({recordCompileOp, prvFileNameToModuleAtom(Filename)}).

prvUnRecordCompileFile(Filename) -> prvCompileRecordingCmd({unRecordCompileOp, prvFileNameToModuleAtom(Filename)}).

recordModule(Filename, FunctionTypeMap) -> prvCompileRecordingCmd({recordModule, prvFileNameToModuleAtom(Filename), FunctionTypeMap}).

prvChangeSearchDirectory(NewDir) -> prvCompileRecordingCmd({change_searchdir, NewDir}).

prvFileNameToModuleAtom(Filename) when is_list(Filename) -> list_to_atom(filename:basename(Filename, ".e")).

prvQueryFunctionInModule(ModName, FunName) ->
    case prvCompileRecordingCmd({queryFunctionReturnType, ModName, FunName}) of
        {error, moduleNotFound, SearchDir} ->
            prvParseAndCompile(prvMakeFileName(SearchDir, ModName)),
            prvQueryFunctionInModule(ModName, FunName);
        {error, functionNotFound} = R ->
            R;
        {ok, _Type} = R ->
            R
    end.

prvMakeFileName(SearchDir, ModName) -> lists:flatten(io_lib:format("~s/~s.e", [SearchDir, ModName])).

prvCompilerRecordingDetails() -> prvCompileRecordingCmd(debug).

prvCompileRecordingCmd(Command) ->
    Ref = erlang:make_ref(),
    ecompilerHelper ! {{self(), Ref}, Command},
    receive {Ref, Result} -> Result end.

prvCompilerRecordingLoop(State) ->
    receive
        {{Pid, Ref}, Command} ->
            case prvCompileRecordingHandle(Command, State) of
                {reply, Result, NewState} ->
                    Pid ! {Ref, Result},
                    prvCompilerRecordingLoop(NewState);
                stop ->
                    Pid ! {Ref, ok}
            end;
        Any ->
            throw(Any)
    end.

prvCompileRecordingHandle({queryFunctionReturnType, ModName, FunName}, #{modmap := ModuleFnMap, searchdir := SearchDir} = State) ->
    case maps:find(ModName, ModuleFnMap) of
        {ok, FunctionTypeMap} ->
            case maps:find(FunName, FunctionTypeMap) of
                {ok, _Type} = D ->  {reply, D, State};
                error ->            {reply, {error, functionNotFound}, State}
            end;
        error ->
            {reply, {error, moduleNotFound, SearchDir}, State}
    end;
prvCompileRecordingHandle({recordCompileOp, ModName}, #{moduleChain := ModuleChain} = State) ->
    NewModuleChain = [ModName | ModuleChain],
    case ecompilerUtil:valueInList(ModName, ModuleChain) of
        true ->
            {reply, {error, moduleRecursive, lists:reverse(NewModuleChain)},
             State};
        _ ->
            {reply, ok, State#{moduleChain := NewModuleChain}}
    end;
prvCompileRecordingHandle({unRecordCompileOp, ModName}, #{moduleChain := [ModName | Rest]} = State) ->
    {reply, ok, State#{moduleChain := Rest}};
prvCompileRecordingHandle({recordModule, ModName, FunctionTypeMap}, #{modmap := ModuleFnMap} = State) ->
    {reply, ok, State#{modmap := ModuleFnMap#{ModName => FunctionTypeMap}}};
prvCompileRecordingHandle({change_searchdir, NewDir}, State) ->
    {reply, ok, State#{searchdir := NewDir}};
prvCompileRecordingHandle(debug, #{modmap := ModuleFnMap, moduleChain := ModuleChain} = State) ->
    {reply, {ModuleFnMap, ModuleChain}, State};
prvCompileRecordingHandle(stop, _) ->
    stop.

-ifdef(EUNIT).

testStartBeforeTest(SearchDir) ->
    prvStopCompilerRecordingProcess(),
    prvStartCompilerRecordingProcess(SearchDir),
    R = prvCompilerRecordingDetails(),
    ?assertEqual(R, {prvInitialModuleASTMap(), []}).

record_test() ->
    testStartBeforeTest("/tmp"),
    ok = prvRecordCompileFile("./a/b/c/file1.e"),
    ok = prvRecordCompileFile("./a/b/c/file2.e"),
    R3 = prvRecordCompileFile("./a/b/c/file1.e"),
    ?assertEqual(R3, {error, moduleRecursive, [file1, file2, file1]}),
    ok.

getUncompiledModule_test() ->
    testStartBeforeTest("./sample"),
    R = prvQueryFunctionInModule(simplemod, test),
    ?assertEqual(R, {ok, {fun_type, 3, [], {basic_type, 3, 0, integer, isize}}}),
    ok.

-endif.