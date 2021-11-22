-module(ecompiler).

-export([compileToAST/1, compileToC/2]).

-include_lib("eunit/include/eunit.hrl").
-include("ecompilerFrameDef.hrl").

-spec compileToC(string(), string()) -> ok.
compileToC(InputFilename, OutputFilename) ->
    try
        {AST, Vars, InitCode} = parseAndCompile(InputFilename),
        %io:format(">> ~p~n~n", [AST]),
        ecompilerGenerateCCode:generateCCode(AST, Vars, InitCode, OutputFilename)
    catch
        {Filename, ErrorInfo} ->
            io:format("~s: ~p~n", [Filename, ErrorInfo])
    end.

-spec compileToAST(string()) -> {eAST(), variableTypeMap(), eAST()}.
compileToAST(Filename) ->
    parseAndCompile(Filename).

-spec parseAndCompile(string()) -> {eAST(), variableTypeMap(), eAST()}.
parseAndCompile(Filename) ->
    try
        {ok, AST} = parseFile(Filename),
        ecompilerCompile:compileFromRawAST(AST, #{})
    catch
        E ->
            throw({Filename, E})
    end.

-spec parseFile(string()) -> {ok, eAST()}.
parseFile(Filename) ->
    case file:read_file(Filename) of
        {ok, RawContent} ->
            case ecompilerScan:string(binary_to_list(RawContent)) of
                {ok, Tokens, _} ->
                    case ecompilerParse:parse(ePreprocess:process(Tokens)) of
                        {ok, _Ast} = D ->
                            D;
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
