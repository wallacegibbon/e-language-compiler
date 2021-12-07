-module(ePreprocess).

-export([process/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type macroMap() :: #{atom() => [token()]}.
-type preprocessContext() :: {MacroMap :: macroMap(), TokensToReturn :: [token()], EndTag :: else | endif | normal}.
-type handleReturn() :: {MacroMap :: macroMap(), TokensToReturn :: [token()], RestTokens :: [token()]}.
-type token() :: any().

-spec handleSpecial([token()], preprocessContext()) -> handleReturn().
handleSpecial([{identifier, _, define}, {identifier, LineNumber, Name} | Rest], {MacroMap, TokensToReturn, EndTag} = Context) ->
    case MacroMap of
        #{Name := _} ->
            throw({LineNumber, eUtil:fmt("macro name conflict: \"~s\"", [Name])});
        _ ->
            {Tokens, RestTokens} = getExpressionTillEOL(Rest, Context),
            handleNormal(RestTokens, {MacroMap#{Name => Tokens}, TokensToReturn, EndTag})
    end;
handleSpecial([{identifier, _, undef}, {identifier, LineNumber, Name} | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    case MacroMap of
        #{Name := _} ->
            handleNormal(Rest, {maps:remove(Name, MacroMap), TokensToReturn, EndTag});
        _ ->
            throw({LineNumber, eUtil:fmt("macro \"~s\" is not defined", [Name])})
    end;
handleSpecial([{identifier, _, ifdef}, {identifier, _, Name} | Rest], {MacroMap, _, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            collectToElseAndIgnoreToEndif(Rest, Context);
                                                        _ ->
                                                            ignoreToElseAndCollectToEndif(Rest, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handleSpecial([{identifier, LineNumber, ifdef} | _], _) ->
    throw({LineNumber, "invalid #ifdef command"});
handleSpecial([{identifier, _, ifndef}, {identifier, _, Name} | Rest], {MacroMap, _, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            ignoreToElseAndCollectToEndif(Rest, Context);
                                                        _ ->
                                                            collectToElseAndIgnoreToEndif(Rest, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handleSpecial([{identifier, LineNumber, ifndef} | _], _) ->
    throw({LineNumber, "invalid #ifndef command"});
handleSpecial([{'if', _} | Rest], {MacroMap, _, EndTag} = Context) ->
    {Tokens, RestTokens} = getExpressionTillEOL(Rest, Context),
    {MacroMapNew, CollectedTokens, RestTokensNew} = case evaluateTokenExpressions(Tokens, MacroMap) of
                                                        true ->
                                                            collectToElseAndIgnoreToEndif(RestTokens, Context);
                                                        false ->
                                                            ignoreToElseAndCollectToEndif(RestTokens, Context)
                                                    end,
    handleNormal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handleSpecial([{else, _} | RestContent], {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, RestContent};
handleSpecial([{else, LineNumber} | _], {_, _, normal}) ->
    throw({LineNumber, "\"#else\" is not expected here"});
handleSpecial([{identifier, _, endif} | RestContent], {MacroMap, TokensToReturn, endif}) ->
    {MacroMap, TokensToReturn, RestContent};
%% when the "#else" part is missing ("#if" following "#endif"), pretend that the "#else\n" exists and has been swallowed,
%% and put the "#endif" back to unhandled tokens.
handleSpecial([{identifier, LineNumber, endif} | _] = Content, {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, [{'#', LineNumber} | Content]};
handleSpecial([{identifier, LineNumber, error} | _], _) ->
    throw({LineNumber, "compile error... (todo)"});
handleSpecial([{identifier, LineNumber, warning} | _], _) ->
    throw({LineNumber, "compile warning... (todo)"});
handleSpecial([{identifier, _, include} | Rest], Context) ->
    {_, RestTokens} = getExpressionTillEOL(Rest, Context),
    handleNormal(RestTokens, Context);
handleSpecial([{identifier, LineNumber, Name} | _], _) ->
    throw({LineNumber, eUtil:fmt("unexpected operator \"~s\" here", [Name])});
handleSpecial([], {MacroMap, TokensToReturn, normal}) ->
    {MacroMap, TokensToReturn, []};
handleSpecial([], {_, _, EndTag}) ->
    throw({0, eUtil:fmt("unexpected end of file while in state: \"#~s\"", [EndTag])}).

-spec collectToElseAndIgnoreToEndif([token()], preprocessContext()) -> handleReturn().
collectToElseAndIgnoreToEndif(Tokens, {MacroMap, TokensToReturn, _}) ->
    %% collect "then" part
    {MacroMapNew, CollectedTokens, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], else}),
    %% ignore "else" part
    {_, _, RestTokens} = handleNormal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens ++ TokensToReturn, RestTokens}.

-spec ignoreToElseAndCollectToEndif([token()], preprocessContext()) -> handleReturn().
ignoreToElseAndCollectToEndif(Tokens, {MacroMap, TokensToReturn, _}) ->
    %% ignore "then" part
    {_, _, RestTokensRaw} = handleNormal(Tokens, {MacroMap, [], else}),
    %% collect "else" part
    {MacroMapNew, CollectedTokens, RestTokens} = handleNormal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens ++ TokensToReturn, RestTokens}.

-spec handleNormal([token()], preprocessContext()) -> handleReturn().
handleNormal([{'?', _}, {identifier, _, _} = Identifier | Rest], {MacroMap, _, _} = Context) ->
    replaceMacro(Identifier, MacroMap, fun (Tokens) -> handleNormal(Tokens ++ Rest, Context) end);
handleNormal([{'?', LineNumber} | _], _) ->
    throw({LineNumber, "syntax error near \"?\""});
handleNormal([{'#', _} | Rest], Context) ->
    handleSpecial(Rest, Context);
handleNormal([{newline, _} | Rest], Context) ->
    handleNormal(Rest, Context);
handleNormal([Token | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    handleNormal(Rest, {MacroMap, [Token | TokensToReturn], EndTag});
handleNormal([], {MacroMap, TokensToReturn, normal}) ->
    {MacroMap, TokensToReturn, []};
handleNormal([], {_, _, EndTag}) ->
    throw({0, eUtil:fmt("unexpected end of file while in state: \"#~s\"", [EndTag])}).

-spec process([token()]) -> [token()].
process(Tokens) ->
    {_, ProcessedTokens, _} = handleNormal(convertElifToElseAndIf(Tokens), {#{}, [], normal}),
    lists:reverse((ProcessedTokens)).

-spec getExpressionTillEOL([token()], preprocessContext()) -> {[token()], [token()]}.
getExpressionTillEOL(Tokens, Context) ->
    getExpressionTillEOL(Tokens, [], Context).

-spec getExpressionTillEOL([token()], [token()], preprocessContext()) -> {[token()], [token()]}.
getExpressionTillEOL([{'?', _}, {identifier, _, _} = Identifier | Rest], CollectedTokens, {MacroMap, _, _} = Context) ->
    replaceMacro(Identifier, MacroMap, fun (Tokens) ->
                                            getExpressionTillEOL(Rest, lists:reverse(Tokens) ++ CollectedTokens, Context)
                                       end);
getExpressionTillEOL([{'?', LineNumber} | _], _, _) ->
    throw({LineNumber, "syntax error near \"?\""});
getExpressionTillEOL([{newline, _} | Rest], CollectedTokens, _) ->
    {lists:reverse(CollectedTokens), Rest};
getExpressionTillEOL([Token | Rest], CollectedTokens, Context) ->
    getExpressionTillEOL(Rest, [Token | CollectedTokens], Context);
%% EOF should not appear in this function, but leave the error handling to upper level.
getExpressionTillEOL([], CollectedTokens, _) ->
    {lists:reverse(CollectedTokens), []}.

-spec replaceMacro({identifier, non_neg_integer(), atom()}, macroMap(), fun(([token()]) -> Result)) -> Result when Result :: any().
replaceMacro({identifier, LineNumber, Name}, MacroMap, ContinueHandler) ->
    case MacroMap of
        #{Name := Value} ->
            ContinueHandler(replaceLineNumber(Value, LineNumber));
        _ ->
            throw({LineNumber, eUtil:fmt("undefined macro ~s", [Name])})
    end.

%% tokens should be parsed to ast before evaluating them, this function will be updated when the parser is finished
evaluateTokenExpressions([{integer, _, 0}], _MacroMap) ->
    false;
evaluateTokenExpressions([{integer, _, 1}], _MacroMap) ->
    true.

-spec replaceLineNumber([token()], non_neg_integer()) -> [token()].
replaceLineNumber(Tokens, LineNumber) ->
    lists:map(fun (Token) -> setelement(2, Token, LineNumber) end, Tokens).

-ifdef(EUNIT).

process_noOperator_test() ->
    {ok, Tokens, _} = eScanner:string("u32 a = 1;"),
    ?assertEqual([{integerType, 1, u32}, {identifier, 1, a}, {'=', 1}, {integer, 1, 1}, {';', 1}], process(Tokens)).

process_if_true_test() ->
    {ok, Tokens, _} = eScanner:string("#if 1\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_if_false_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_ifdef_false_test() ->
    {ok, Tokens, _} = eScanner:string("#ifdef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_ifdef_true_test() ->
    {ok, Tokens, _} = eScanner:string("#define BLAH\n #ifdef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 3, a}], process(Tokens)).

process_ifndef_false_test() ->
    {ok, Tokens, _} = eScanner:string("#ifndef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_ifndef_true_test() ->
    {ok, Tokens, _} = eScanner:string("#define BLAH\n #ifndef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 5, b}], process(Tokens)).

process_recursive_1_test() ->
    {ok, Tokens, _} = eScanner:string("#if 1\n #if 1\n a\n #else\n b\n #endif\n #else\n c\n #endif"),
    ?assertEqual([{identifier, 3, a}], process(Tokens)).

process_recursive_2_test() ->
    {ok, Tokens, _} = eScanner:string("#if 1\n #if 0\n a\n #else\n b\n #endif\n #else\n c\n #endif"),
    ?assertEqual([{identifier, 5, b}], process(Tokens)).

process_recursive_3_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n #if 1\n a\n #else\n b\n #endif\n #else\n c\n #endif"),
    ?assertEqual([{identifier, 8, c}], process(Tokens)).

process_elif_1_test() ->
    {ok, Tokens, _} = eScanner:string("#if 1\n a\n #elif 1\n b\n #endif"),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_elif_2_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n a\n #elif 0\n b\n #elif 0\n c\n #else\n d\n #endif"),
    ?assertEqual([{identifier, 8, d}], process(Tokens)).

process_elif_3_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif"),
    ?assertEqual([{identifier, 6, c}], process(Tokens)).

process_elif_4_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n a\n #elif 1\n b\n #elif 1\n c\n #else\n d\n #endif"),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_define_lineNumber_test() ->
    {ok, Tokens, _} = eScanner:string("#define A 1\n?A"),
    ?assertEqual([{integer, 2, 1}], process(Tokens)).

process_define_lineNumber_2_test() ->
    {ok, Tokens, _} = eScanner:string("#define A 1\n#define B ?A\n?B"),
    ?assertEqual([{integer, 3, 1}], process(Tokens)).

process_define_1_test() ->
    {ok, Tokens, _} = eScanner:string("#define A 1 + 2 + 3\n?A + 1"),
    ?assertEqual("1 + 2 + 3 + 1", tokensToString(process(Tokens))).

process_define_2_test() ->
    {ok, Tokens, _} = eScanner:string("#define A (1 + 2 + 3)\n?A + 1"),
    ?assertEqual("( 1 + 2 + 3 ) + 1", tokensToString(process(Tokens))).

process_define_3_test() ->
    {ok, Tokens, _} = eScanner:string("#define A 1 + 2 + 3\n#define B ?A + 4\n?B + 5"),
    ?assertEqual("1 + 2 + 3 + 4 + 5", tokensToString(process(Tokens))).

process_undef_1_test() ->
    {ok, Tokens, _} = eScanner:string("#define A 1 + 2 + 3\n#undef A\n?A + 1"),
    ?assertThrow({3, "undefined macro A"}, tokensToString(process(Tokens))).

process_undef_2_test() ->
    {ok, Tokens, _} = eScanner:string("#define A 1 + 2 + 3\n#undef B\n?A + 1"),
    ?assertThrow({2, "macro \"B\" is not defined"}, tokensToString(process(Tokens))).

-endif.

-spec convertElifToElseAndIf([token()]) -> [token()].
convertElifToElseAndIf(Tokens) ->
    lists:flatten(convertElifToElseAndIf(Tokens, 0)).

-spec convertElifToElseAndIf([token()], integer()) -> TokenTree when TokenTree :: [token() | TokenTree].
convertElifToElseAndIf([{'#', _} = PreTag, {'if', _} = Token | Rest], _) ->
    [PreTag, Token | convertElifToElseAndIf(Rest, 0)];
convertElifToElseAndIf([{'#', _}, {elif, LineNumber} | Rest], ElifDepth) ->
    [makeElifReplacement(LineNumber) | convertElifToElseAndIf(Rest, ElifDepth + 1)];
convertElifToElseAndIf([{'#', _} = PreTag, {identifier, _, endif} = Token | Rest], ElifDepth) ->
    [lists:duplicate(ElifDepth + 1, [PreTag, Token]) | convertElifToElseAndIf(Rest, 0)];
convertElifToElseAndIf([Token | Rest], ElifDepth) ->
    [Token | convertElifToElseAndIf(Rest, ElifDepth)];
convertElifToElseAndIf([], N) when N =/= 0 ->
    throw({0, "preprocessor error, #if and #endif mismatch"});
convertElifToElseAndIf([], 0) ->
    [].

-spec makeElifReplacement(integer()) -> [any()].
makeElifReplacement(LineNumber) ->
    [{'#', LineNumber}, {else, LineNumber}, {'#', LineNumber}, {'if', LineNumber}].

-ifdef(EUNIT).

convertElifToElseAndIf_1_test() ->
    {ok, Tokens, _} = eScanner:string("#if 1\n a\n #elif 1\n b\n #endif"),
    ?assertEqual("# if 1 \n a \n # else # if 1 \n b \n # endif # endif",
                 tokensToString(convertElifToElseAndIf(Tokens))).

convertElifToElseAndIf_2_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif"),
    ?assertEqual("# if 0 \n a \n # else # if 0 \n b \n # else # if 1 \n c \n # else \n d \n # endif # endif # endif",
                 tokensToString(convertElifToElseAndIf(Tokens))).

convertElifToElseAndIf_3_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n #else\n #endif"),
    ?assertEqual("# if 0 \n # else \n # endif",
                 tokensToString(convertElifToElseAndIf(Tokens))).

convertElifToElseAndIf_4_test() ->
    {ok, Tokens, _} = eScanner:string("#if 0\n #if 1\n #else\n #endif #endif"),
    ?assertEqual("# if 0 \n # if 1 \n # else \n # endif # endif",
                 tokensToString(convertElifToElseAndIf(Tokens))).

-spec tokensToString([token()]) -> string().
tokensToString(Tokens) ->
    lists:flatten(lists:join(" ", lists:map(fun tokenToString/1, Tokens))).

-spec tokenToString(token()) -> string().
tokenToString({integer, _, Number}) ->
    integer_to_list(Number);
tokenToString({float, _, Number}) ->
    float_to_list(Number);
tokenToString({string, _, String}) ->
    String;
tokenToString({identifier, _, Name}) ->
    atom_to_list(Name);
tokenToString({newline, _}) ->
    $\n;
tokenToString({AnyAtom, _}) ->
    atom_to_list(AnyAtom).

tokensToString_test() ->
    ?assertEqual("* + \n \n", tokensToString([{'*', 1}, {'+', 1}, {newline, 1}, {newline,2}])).

-endif.