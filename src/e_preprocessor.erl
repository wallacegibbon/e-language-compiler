-module(e_preprocessor).
-export([preprocess/1]).
-include("e_record_definition.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type macro_map() :: #{atom() => [token()]}.
-type context() :: #{macro_map := macro_map(), macro_path := [atom()]}.

-spec preprocess([token()]) -> [token()].
preprocess(OrigTokens) ->
    {Map, FilteredTokens} = collect_macro_map(OrigTokens, [], #{}),
    replace_macro(FilteredTokens, [], #{macro_map => Map, macro_path => []}).

collect_macro_map([{'#', _}, {identifier, _, define}, {identifier, Loc, Name} | Rest],
                  FilteredTokens,
                  Map) ->
    case maps:find(Name, Map) of
        {ok, _} ->
            e_util:ethrow(Loc, "macro name conflict: \"~s\"", [Name]);
        _ ->
            {Tokens, RestTokens} = get_expr_till_eol(Rest, []),
            collect_macro_map(RestTokens, FilteredTokens, Map#{Name => Tokens})
    end;
collect_macro_map([{'#', _}, {identifier, Loc, Name} | _], _, _) ->
    e_util:ethrow(Loc, "unexpected preprocessor operator \"~s\"", [Name]);
collect_macro_map([Token | Rest], FilteredTokens, Map) ->
    collect_macro_map(Rest, [Token | FilteredTokens], Map);
collect_macro_map([], FilteredTokens, Map) ->
    {Map, lists:reverse(FilteredTokens)}.

-spec replace_macro([token()], [token()], context()) -> [token()].
replace_macro([{'?', Loc}, {identifier, _, Name} = I | Rest], Ret,
              #{macro_map := Map, macro_path := Path} = Ctx) ->
    case lists:member(Name, Path) of
        true ->
            Str = string:join([atom_to_list(A) || A <- lists:reverse(Path, [Name])], "->"),
            e_util:ethrow(Loc, "recursive macro expanding: ~s~n", [Str]);
        false ->
            Replaced = replace_macro(do_replace(I, Map), [], Ctx#{macro_path := [Name | Path]}),
            replace_macro(Replaced ++ Rest, Ret, Ctx)
    end;
replace_macro([{'?', Loc} | _], _, _) ->
    e_util:ethrow(Loc, "syntax error near \"?\"");
replace_macro([{newline, _} | Rest], Ret, Ctx) ->
    replace_macro(Rest, Ret, Ctx);
replace_macro([Token | Rest], Ret, Ctx) ->
    replace_macro(Rest, [Token | Ret], Ctx);
replace_macro([], Ret, _) ->
    lists:reverse(Ret).

-spec get_expr_till_eol([token()], [token()]) -> {[token()], [token()]}.
get_expr_till_eol([{newline, _} | Rest], Tokens) ->
    {lists:reverse(Tokens), Rest};
get_expr_till_eol([Token | Rest], Tokens) ->
    get_expr_till_eol(Rest, [Token | Tokens]);
get_expr_till_eol([], Tokens) ->
    {lists:reverse(Tokens), []}.

-spec do_replace({identifier, location(), atom()}, macro_map()) -> [token()].
do_replace({identifier, Loc, Name}, Map) ->
    case maps:find(Name, Map) of
        {ok, Value} ->
            replace_line_number(Value, Loc);
        _ ->
            e_util:ethrow(Loc, "undefined macro ~s", [Name])
    end.

-spec replace_line_number([token()], location()) -> [token()].
replace_line_number(Tokens, Loc) ->
    [setelement(2, Token, Loc) || Token <- Tokens].

-ifdef(EUNIT).

scan_string(Content) ->
    {ok, Tokens, _} = e_scanner:string(Content),
    [e_util:token_attach_filename("", T) || T <- Tokens].

process_define_line_number_1_test() ->
    Tokens = scan_string("#define A 1\n?A"),
    ?assertMatch([{integer, {_, 2, 2}, 1}], preprocess(Tokens)).

process_define_line_number_2_test() ->
    Tokens = scan_string("#define A 1\n#define B ?A\n?B"),
    ?assertMatch([{integer, {_, 3, 2}, 1}], preprocess(Tokens)).

process_define_1_test() ->
    Tokens = scan_string("#define A 1 + 2 + 3\n?A + 1"),
    ?assertEqual("1 + 2 + 3 + 1", tokens_to_str(preprocess(Tokens))).

process_define_2_test() ->
    Tokens = scan_string("#define A (1 + 2 + 3)\n?A + 1"),
    ?assertEqual("( 1 + 2 + 3 ) + 1", tokens_to_str(preprocess(Tokens))).

process_define_3_test() ->
    Tokens = scan_string("#define A 1 + 2 + 3\n#define B ?A + 4\n?B + 5"),
    ?assertEqual("1 + 2 + 3 + 4 + 5", tokens_to_str(preprocess(Tokens))).

process_define_4_test() ->
    Tokens = scan_string("#define A ?B\n#define B ?C\n#define C ?A + 1\n?A"),
    ?assertThrow({_, "recursive macro expanding: A" ++ _},
                 tokens_to_str(preprocess(Tokens))).

process_define_5_test() ->
    Tokens = scan_string("#define A ?B\n#define B ?C\n#define C ?A + 1\n?B"),
    ?assertThrow({_, "recursive macro expanding: B" ++ _},
                 tokens_to_str(preprocess(Tokens))).

process_define_6_test() ->
    Tokens = scan_string("#define A ?B\n#define B ?C\n#define C ?A + 1\n?C"),
    ?assertThrow({_, "recursive macro expanding: C" ++ _},
                 tokens_to_str(preprocess(Tokens))).

process_define_7_test() ->
    Tokens = scan_string("#define A 1\n#define B ?A\n?B\n?B"),
    ?assertMatch([{integer, {_, 3, 2}, 1},
                  {integer, {_, 4, 2}, 1}], preprocess(Tokens)).

tokens_to_str(Tokens) ->
    lists:flatten(lists:join(" ", [token_to_str(T) || T <- Tokens])).

-spec token_to_str(token()) -> string().
token_to_str({integer   , _, Number}) -> integer_to_list(Number);
token_to_str({float     , _, Number}) -> float_to_list(Number);
token_to_str({string    , _, String}) -> String;
token_to_str({identifier, _, Name  }) -> atom_to_list(Name);
token_to_str({newline   , _        }) -> $\n;
token_to_str({AnyAtom   , _        }) -> atom_to_list(AnyAtom).

tokens_to_str_test() ->
    Pos = {0, 0},
    ?assertEqual("* + \n \n",
                 tokens_to_str([{'*', Pos}, {'+', Pos},
                                {newline, Pos}, {newline, Pos}])).

-endif.
