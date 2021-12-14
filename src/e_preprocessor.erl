-module(e_preprocessor).
-export([process/1]).

-type macro_map() :: #{atom() => [token()]}.
-type context() :: {MacroMap :: macro_map(), TokensToReturn :: [token()], EndTag :: else | endif | normal}.
-type handle_ret() :: {MacroMap :: macro_map(), TokensToReturn :: [token()], RestTokens :: [token()]}.
-type token() :: any().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec handle_special([token()], context()) -> handle_ret().
handle_special([{identifier, _, define}, {identifier, LineNumber, Name} | Rest], {MacroMap, TokensToReturn, EndTag} = Context) ->
    case MacroMap of
        #{Name := _} ->
            throw({LineNumber, e_util:fmt("macro name conflict: \"~s\"", [Name])});
        _ ->
            {Tokens, RestTokens} = get_expr_till_eol(Rest, Context),
            handle_normal(RestTokens, {MacroMap#{Name => Tokens}, TokensToReturn, EndTag})
    end;
handle_special([{identifier, _, undef}, {identifier, LineNumber, Name} | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    case MacroMap of
        #{Name := _} ->
            handle_normal(Rest, {maps:remove(Name, MacroMap), TokensToReturn, EndTag});
        _ ->
            throw({LineNumber, e_util:fmt("macro \"~s\" is not defined", [Name])})
    end;
handle_special([{identifier, _, ifdef}, {identifier, _, Name} | Rest], {MacroMap, _, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            collect_to_else_and_ignore_to_endif(Rest, Context);
                                                        _ ->
                                                            ignore_to_else_and_collect_to_endif(Rest, Context)
                                                    end,
    handle_normal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handle_special([{identifier, LineNumber, ifdef} | _], _) ->
    throw({LineNumber, "invalid #ifdef command"});
handle_special([{identifier, _, ifndef}, {identifier, _, Name} | Rest], {MacroMap, _, EndTag} = Context) ->
    {MacroMapNew, CollectedTokens, RestTokensNew} = case MacroMap of
                                                        #{Name := _} ->
                                                            ignore_to_else_and_collect_to_endif(Rest, Context);
                                                        _ ->
                                                            collect_to_else_and_ignore_to_endif(Rest, Context)
                                                    end,
    handle_normal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handle_special([{identifier, LineNumber, ifndef} | _], _) ->
    throw({LineNumber, "invalid #ifndef command"});
handle_special([{'if', _} | Rest], {MacroMap, _, EndTag} = Context) ->
    {Tokens, RestTokens} = get_expr_till_eol(Rest, Context),
    {MacroMapNew, CollectedTokens, RestTokensNew} = case eval_token_exprs(Tokens, MacroMap) of
                                                        true ->
                                                            collect_to_else_and_ignore_to_endif(RestTokens, Context);
                                                        false ->
                                                            ignore_to_else_and_collect_to_endif(RestTokens, Context)
                                                    end,
    handle_normal(RestTokensNew, {MacroMapNew, CollectedTokens, EndTag});
handle_special([{else, _} | RestContent], {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, RestContent};
handle_special([{else, LineNumber} | _], {_, _, normal}) ->
    throw({LineNumber, "\"#else\" is not expected here"});
handle_special([{identifier, _, endif} | RestContent], {MacroMap, TokensToReturn, endif}) ->
    {MacroMap, TokensToReturn, RestContent};
%% when the "#else" part is missing ("#if" following "#endif"), pretend that the "#else\n" exists and has been swallowed,
%% and put the "#endif" back to unhandled tokens.
handle_special([{identifier, LineNumber, endif} | _] = Content, {MacroMap, TokensToReturn, else}) ->
    {MacroMap, TokensToReturn, [{'#', LineNumber} | Content]};
handle_special([{identifier, LineNumber, error} | _], _) ->
    throw({LineNumber, "compile error... (todo)"});
handle_special([{identifier, LineNumber, warning} | _], _) ->
    throw({LineNumber, "compile warning... (todo)"});
handle_special([{identifier, _, include} | Rest], Context) ->
    {_, RestTokens} = get_expr_till_eol(Rest, Context),
    handle_normal(RestTokens, Context);
handle_special([{identifier, LineNumber, Name} | _], _) ->
    throw({LineNumber, e_util:fmt("unexpected operator \"~s\" here", [Name])});
handle_special([], {MacroMap, TokensToReturn, normal}) ->
    {MacroMap, TokensToReturn, []};
handle_special([], {_, _, EndTag}) ->
    throw({0, e_util:fmt("unexpected end of file while in state: \"#~s\"", [EndTag])}).

-spec collect_to_else_and_ignore_to_endif([token()], context()) -> handle_ret().
collect_to_else_and_ignore_to_endif(Tokens, {MacroMap, TokensToReturn, _}) ->
    %% collect "then" part
    {MacroMapNew, CollectedTokens, RestTokensRaw} = handle_normal(Tokens, {MacroMap, [], else}),
    %% ignore "else" part
    {_, _, RestTokens} = handle_normal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens ++ TokensToReturn, RestTokens}.

-spec ignore_to_else_and_collect_to_endif([token()], context()) -> handle_ret().
ignore_to_else_and_collect_to_endif(Tokens, {MacroMap, TokensToReturn, _}) ->
    %% ignore "then" part
    {_, _, RestTokensRaw} = handle_normal(Tokens, {MacroMap, [], else}),
    %% collect "else" part
    {MacroMapNew, CollectedTokens, RestTokens} = handle_normal(RestTokensRaw, {MacroMap, [], endif}),
    {MacroMapNew, CollectedTokens ++ TokensToReturn, RestTokens}.

-spec handle_normal([token()], context()) -> handle_ret().
handle_normal([{'?', _}, {identifier, _, _} = Identifier | Rest], {MacroMap, _, _} = Context) ->
    replace_macro(Identifier, MacroMap, fun (Tokens) -> handle_normal(Tokens ++ Rest, Context) end);
handle_normal([{'?', LineNumber} | _], _) ->
    throw({LineNumber, "syntax error near \"?\""});
handle_normal([{'#', _} | Rest], Context) ->
    handle_special(Rest, Context);
handle_normal([{newline, _} | Rest], Context) ->
    handle_normal(Rest, Context);
handle_normal([Token | Rest], {MacroMap, TokensToReturn, EndTag}) ->
    handle_normal(Rest, {MacroMap, [Token | TokensToReturn], EndTag});
handle_normal([], {MacroMap, TokensToReturn, normal}) ->
    {MacroMap, TokensToReturn, []};
handle_normal([], {_, _, EndTag}) ->
    throw({0, e_util:fmt("unexpected end of file while in state: \"#~s\"", [EndTag])}).

-spec process([token()]) -> [token()].
process(Tokens) ->
    {_, ProcessedTokens, _} = handle_normal(convert_elif_to_else_and_if(Tokens), {#{}, [], normal}),
    lists:reverse((ProcessedTokens)).

-spec get_expr_till_eol([token()], context()) -> {[token()], [token()]}.
get_expr_till_eol(Tokens, Context) ->
    get_expr_till_eol(Tokens, [], Context).

-spec get_expr_till_eol([token()], [token()], context()) -> {[token()], [token()]}.
get_expr_till_eol([{'?', _}, {identifier, _, _} = Identifier | Rest], CollectedTokens, {MacroMap, _, _} = Context) ->
    replace_macro(Identifier, MacroMap, fun (Tokens) ->
                                                get_expr_till_eol(Rest, lists:reverse(Tokens) ++ CollectedTokens, Context)
                                        end);
get_expr_till_eol([{'?', LineNumber} | _], _, _) ->
    throw({LineNumber, "syntax error near \"?\""});
get_expr_till_eol([{newline, _} | Rest], CollectedTokens, _) ->
    {lists:reverse(CollectedTokens), Rest};
get_expr_till_eol([Token | Rest], CollectedTokens, Context) ->
    get_expr_till_eol(Rest, [Token | CollectedTokens], Context);
%% EOF should not appear in this function, but leave the error handling to upper level.
get_expr_till_eol([], CollectedTokens, _) ->
    {lists:reverse(CollectedTokens), []}.

-spec replace_macro({identifier, non_neg_integer(), atom()}, macro_map(), fun(([token()]) -> Result)) -> Result when Result :: any().
replace_macro({identifier, LineNumber, Name}, MacroMap, ContinueHandler) ->
    case MacroMap of
        #{Name := Value} ->
            ContinueHandler(replace_line_number(Value, LineNumber));
        _ ->
            throw({LineNumber, e_util:fmt("undefined macro ~s", [Name])})
    end.

%% tokens should be parsed to ast before evaluating them, this function will be updated when the parser is finished
eval_token_exprs([{integer, _, 0}], _MacroMap) ->
    false;
eval_token_exprs([{integer, _, 1}], _MacroMap) ->
    true.

-spec replace_line_number([token()], non_neg_integer()) -> [token()].
replace_line_number(Tokens, LineNumber) ->
    lists:map(fun (Token) -> setelement(2, Token, LineNumber) end, Tokens).

-ifdef(EUNIT).

process_no_operator_test() ->
    {ok, Tokens, _} = e_scanner:string("u32 a = 1;"),
    ?assertEqual([{integerType, 1, u32}, {identifier, 1, a}, {'=', 1}, {integer, 1, 1}, {';', 1}], process(Tokens)).

process_if_true_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 1\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_if_false_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_ifdef_false_test() ->
    {ok, Tokens, _} = e_scanner:string("#ifdef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_ifdef_true_test() ->
    {ok, Tokens, _} = e_scanner:string("#define BLAH\n #ifdef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 3, a}], process(Tokens)).

process_ifndef_false_test() ->
    {ok, Tokens, _} = e_scanner:string("#ifndef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_ifndef_true_test() ->
    {ok, Tokens, _} = e_scanner:string("#define BLAH\n #ifndef BLAH\n a\n #else\n b\n #endif"),
    ?assertEqual([{identifier, 5, b}], process(Tokens)).

process_recursive_1_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 1\n #if 1\n a\n #else\n b\n #endif\n #else\n c\n #endif"),
    ?assertEqual([{identifier, 3, a}], process(Tokens)).

process_recursive_2_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 1\n #if 0\n a\n #else\n b\n #endif\n #else\n c\n #endif"),
    ?assertEqual([{identifier, 5, b}], process(Tokens)).

process_recursive_3_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n #if 1\n a\n #else\n b\n #endif\n #else\n c\n #endif"),
    ?assertEqual([{identifier, 8, c}], process(Tokens)).

process_elif_1_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 1\n a\n #elif 1\n b\n #endif"),
    ?assertEqual([{identifier, 2, a}], process(Tokens)).

process_elif_2_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n a\n #elif 0\n b\n #elif 0\n c\n #else\n d\n #endif"),
    ?assertEqual([{identifier, 8, d}], process(Tokens)).

process_elif_3_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif"),
    ?assertEqual([{identifier, 6, c}], process(Tokens)).

process_elif_4_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n a\n #elif 1\n b\n #elif 1\n c\n #else\n d\n #endif"),
    ?assertEqual([{identifier, 4, b}], process(Tokens)).

process_define_line_number_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A 1\n?A"),
    ?assertEqual([{integer, 2, 1}], process(Tokens)).

process_define_line_number_2_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A 1\n#define B ?A\n?B"),
    ?assertEqual([{integer, 3, 1}], process(Tokens)).

process_define_1_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A 1 + 2 + 3\n?A + 1"),
    ?assertEqual("1 + 2 + 3 + 1", tokens_to_str(process(Tokens))).

process_define_2_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A (1 + 2 + 3)\n?A + 1"),
    ?assertEqual("( 1 + 2 + 3 ) + 1", tokens_to_str(process(Tokens))).

process_define_3_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A 1 + 2 + 3\n#define B ?A + 4\n?B + 5"),
    ?assertEqual("1 + 2 + 3 + 4 + 5", tokens_to_str(process(Tokens))).

process_undef_1_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A 1 + 2 + 3\n#undef A\n?A + 1"),
    ?assertThrow({3, "undefined macro A"}, tokens_to_str(process(Tokens))).

process_undef_2_test() ->
    {ok, Tokens, _} = e_scanner:string("#define A 1 + 2 + 3\n#undef B\n?A + 1"),
    ?assertThrow({2, "macro \"B\" is not defined"}, tokens_to_str(process(Tokens))).

-endif.

-spec convert_elif_to_else_and_if([token()]) -> [token()].
convert_elif_to_else_and_if(Tokens) ->
    lists:flatten(convert_elif_to_else_and_if(Tokens, 0)).

-spec convert_elif_to_else_and_if([token()], integer()) -> TokenTree when TokenTree :: [token() | TokenTree].
convert_elif_to_else_and_if([{'#', _} = PreTag, {'if', _} = Token | Rest], _) ->
    [PreTag, Token | convert_elif_to_else_and_if(Rest, 0)];
convert_elif_to_else_and_if([{'#', _}, {elif, LineNumber} | Rest], ElifDepth) ->
    [mk_elif_replacement(LineNumber) | convert_elif_to_else_and_if(Rest, ElifDepth + 1)];
convert_elif_to_else_and_if([{'#', _} = PreTag, {identifier, _, endif} = Token | Rest], ElifDepth) ->
    [lists:duplicate(ElifDepth + 1, [PreTag, Token]) | convert_elif_to_else_and_if(Rest, 0)];
convert_elif_to_else_and_if([Token | Rest], ElifDepth) ->
    [Token | convert_elif_to_else_and_if(Rest, ElifDepth)];
convert_elif_to_else_and_if([], N) when N =/= 0 ->
    throw({0, "preprocessor error, #if and #endif mismatch"});
convert_elif_to_else_and_if([], 0) ->
    [].

-spec mk_elif_replacement(integer()) -> [any()].
mk_elif_replacement(LineNumber) ->
    [{'#', LineNumber}, {else, LineNumber}, {'#', LineNumber}, {'if', LineNumber}].

-ifdef(EUNIT).

convert_elif_to_else_and_if_1_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 1\n a\n #elif 1\n b\n #endif"),
    ?assertEqual("# if 1 \n a \n # else # if 1 \n b \n # endif # endif",
                 tokens_to_str(convert_elif_to_else_and_if(Tokens))).

convert_elif_to_else_and_if_2_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n a\n #elif 0\n b\n #elif 1\n c\n #else\n d\n #endif"),
    ?assertEqual("# if 0 \n a \n # else # if 0 \n b \n # else # if 1 \n c \n # else \n d \n # endif # endif # endif",
                 tokens_to_str(convert_elif_to_else_and_if(Tokens))).

convert_elif_to_else_and_if_3_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n #else\n #endif"),
    ?assertEqual("# if 0 \n # else \n # endif",
                 tokens_to_str(convert_elif_to_else_and_if(Tokens))).

convert_elif_to_else_and_if_4_test() ->
    {ok, Tokens, _} = e_scanner:string("#if 0\n #if 1\n #else\n #endif #endif"),
    ?assertEqual("# if 0 \n # if 1 \n # else \n # endif # endif",
                 tokens_to_str(convert_elif_to_else_and_if(Tokens))).

-spec tokens_to_str([token()]) -> string().
tokens_to_str(Tokens) ->
    lists:flatten(lists:join(" ", lists:map(fun token_to_str/1, Tokens))).

-spec token_to_str(token()) -> string().
token_to_str({integer, _, Number}) ->
    integer_to_list(Number);
token_to_str({float, _, Number}) ->
    float_to_list(Number);
token_to_str({string, _, String}) ->
    String;
token_to_str({identifier, _, Name}) ->
    atom_to_list(Name);
token_to_str({newline, _}) ->
    $\n;
token_to_str({AnyAtom, _}) ->
    atom_to_list(AnyAtom).

tokens_to_str_test() ->
    ?assertEqual("* + \n \n", tokens_to_str([{'*', 1}, {'+', 1}, {newline, 1}, {newline,2}])).

-endif.