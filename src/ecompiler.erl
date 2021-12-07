-module(ecompiler).

-export([compileToAST/1, compileToC/2]).

-include_lib("eunit/include/eunit.hrl").
-include("eRecordDefinition.hrl").

-spec compileToC(string(), string()) -> ok.
compileToC(InputFilename, OutputFilename) ->
    try
        {AST, Vars, InitCode} = parseAndCompile(InputFilename),
        %io:format(">> ~p~n~n", [AST]),
        cCodeGenerator:generateCCode(AST, Vars, InitCode, OutputFilename)
    catch
        {Filename, ErrorInfo} ->
            io:format("~s: ~p~n", [Filename, ErrorInfo])
    end.

-spec compileToAST(string()) -> {eAST(), variableTypeMap(), eAST()}.
compileToAST(Filename) ->
    parseAndCompile(Filename).

-spec parseAndCompile(string()) -> {eAST(), variableTypeMap(), eAST()}.
parseAndCompile(Filename) ->
    try eMainCompiler:compileFromRawAST(parseFile(Filename), #{})
    catch
        E ->
            throw({Filename, E})
    end.

-spec parseFile(string()) -> eAST().
parseFile(Filename) ->
    case file:read_file(Filename) of
        {ok, RawContent} ->
            case eScanner:string(binary_to_list(RawContent)) of
                {ok, Tokens, _} ->
                    case eParser:parse(ePreprocessor:process(Tokens)) of
                        {ok, AST} ->
                            AST;
                        {error, {Line, _, ErrorInfo}} ->
                            throw({Line, ErrorInfo})
                    end;
                {error, ErrorInfo, Line} ->
                    throw({Line, ErrorInfo})
            end;
        {error, enoent} ->
            throw("module not found");
        {error, Reason} ->
            throw(Reason)
    end.
