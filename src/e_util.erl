-module(e_util).
-export([make_function_and_struct_map_from_ast/1, expr_map/2, eliminate_pointer/1, stmt_to_str/1, merge_vars/3]).
-export([names_of_var_defs/1, names_of_var_refs/1, get_struct_from_type/2, get_struct_from_name/3, void_type/1]).
-export([fall_unit/2, fill_unit_opti/2, fill_unit_pessi/2, fix_special_chars/1]).
-export([fmt/2, ethrow/3, ethrow/2, assert/2, get_values_by_keys/2, get_kvpair_by_keys/2, map_find_multi/2]).
-export([u_type_immedi/1, j_type_immedi/1, s_type_immedi/1, b_type_immedi/1]).
-export([list_map/2, file_write/2]).
-include("e_record_definition.hrl").
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% This function is to avoid boilerplate code for statements. So you can concentrate on operators.
-spec expr_map(fun((e_expr()) -> e_expr()), [e_stmt()]) -> [e_stmt()].
expr_map(Fn, [#e_if_stmt{'cond' = Cond, then = Then, 'else' = Else} = If | Rest]) ->
	[If#e_if_stmt{'cond' = Fn(Cond), then = expr_map(Fn, Then), 'else' = expr_map(Fn, Else)} | expr_map(Fn, Rest)];
expr_map(Fn, [#e_while_stmt{'cond' = Cond, stmts = Stmts} = While | Rest]) ->
	[While#e_while_stmt{'cond' = Fn(Cond), stmts = expr_map(Fn, Stmts)} | expr_map(Fn, Rest)];
expr_map(Fn, [#e_return_stmt{expr = Expr} = Ret | Rest]) ->
	[Ret#e_return_stmt{expr = Fn(Expr)} | expr_map(Fn, Rest)];
expr_map(Fn, [Any | Rest]) ->
	[Fn(Any) | expr_map(Fn, Rest)];
expr_map(_, []) ->
	[].

-spec eliminate_pointer([e_stmt()]) -> [e_stmt()].
eliminate_pointer(Stmts1) ->
	expr_map(fun merge_plus/1, expr_map(fun merge_pointer/1, Stmts1)).

-spec merge_plus(e_expr()) -> e_expr().
%% Handling expressions like `1 + 2 + 3 + ...`
merge_plus(?OP2('+', ?OP2('+', O1, ?I(N1) = I), ?I(N2)) = Orig) ->
	merge_plus(Orig?OP2('+', O1, I?I(N1 + N2)));
%% Handling expressions like `... + (1 + (2 + 3))`
merge_plus(?OP2('+', ?I(N1) = I, ?OP2('+', ?I(N2), O2)) = Orig) ->
	merge_plus(Orig?OP2('+', I?I(N1 + N2), O2));
merge_plus(?OP2('+', ?I(N1) = I, ?I(N2))) ->
	I?I(N1 + N2);
merge_plus(?CALL(Callee, Args) = Op) ->
	Op?CALL(merge_plus(Callee), lists:map(fun merge_plus/1, Args));
merge_plus(#e_op{data = Operands} = Op) ->
	Op#e_op{data = lists:map(fun merge_plus/1, Operands)};
merge_plus(Any) ->
	Any.

-spec merge_pointer(e_expr()) -> e_expr().
merge_pointer(?OP2('^', ?OP1('@', E), _)) ->
	merge_pointer(E);
merge_pointer(?OP1('@', ?OP2('^', E, _))) ->
	merge_pointer(E);
merge_pointer(?CALL(Callee, Args) = Op) ->
	Op?CALL(merge_pointer(Callee), lists:map(fun merge_pointer/1, Args));
merge_pointer(#e_op{data = Operands} = Op) ->
	Op#e_op{data = lists:map(fun merge_pointer/1, Operands)};
merge_pointer(Any) ->
	Any.

-spec stmt_to_str(e_stmt()) -> string().
stmt_to_str(#e_if_stmt{'cond' = Cond, then = Then, 'else' = Else}) ->
	io_lib:format("if ~s then ~s else ~s end", [stmt_to_str(Cond), lists:map(fun stmt_to_str/1, Then), lists:map(fun stmt_to_str/1, Else)]);
stmt_to_str(#e_while_stmt{'cond' = Cond, stmts = Stmts}) ->
	io_lib:format("while ~s do ~s end", [stmt_to_str(Cond), lists:map(fun stmt_to_str/1, Stmts)]);
stmt_to_str(#e_return_stmt{expr = Expr}) ->
	io_lib:format("return (~s)", [stmt_to_str(Expr)]);
stmt_to_str(#e_goto_stmt{label = Label}) ->
	io_lib:format("goto ~s", [Label]);
stmt_to_str(#e_label{name = Name}) ->
	io_lib:format("@@~s", [Name]);
stmt_to_str(#e_varref{name = Name}) ->
	atom_to_list(Name);
stmt_to_str(#e_vardef{name = Name}) ->
	io_lib:format("variable definition for ~s", [Name]);
stmt_to_str(?CALL(Callee, Args)) ->
	io_lib:format("(~s)(~s)", [stmt_to_str(Callee), string:join(lists:map(fun stmt_to_str/1, Args), ",")]);
stmt_to_str(?OP2('^', Op1, _Size)) ->
	%io_lib:format("(~s^ <size:~s>)", [stmt_to_str(Op1), stmt_to_str(_Size)]);
	io_lib:format("((~s)^)", [stmt_to_str(Op1)]);
stmt_to_str(?OP2(Tag, Op1, Op2)) ->
	io_lib:format("(~s ~s ~s)", [stmt_to_str(Op1), Tag, stmt_to_str(Op2)]);
stmt_to_str(?OP1('not', Op)) ->
	io_lib:format("not(~s)", [stmt_to_str(Op)]);
stmt_to_str(?OP1('bnot', Op)) ->
	io_lib:format("bnot(~s)", [stmt_to_str(Op)]);
stmt_to_str(?OP1(Tag, Op)) ->
	io_lib:format("(~s ~s)", [stmt_to_str(Op), Tag]);
stmt_to_str(#e_array_init_expr{elements = Elements}) ->
	ElementStr = string:join(lists:map(fun(?I(V)) -> integer_to_list(V) end, Elements), ","),
	io_lib:format("{~s}", [ElementStr]);
stmt_to_str(#e_struct_init_expr{name = Name}) ->
	io_lib:format("{...} (struct ~s init expr)", [Name]);
stmt_to_str(?I(Val)) ->
	io_lib:format("~w", [Val]);
stmt_to_str(?F(Val)) ->
	io_lib:format("~w", [Val]);
stmt_to_str(#e_string{value = Val}) ->
	io_lib:format("\"~s\"", [fix_special_chars(Val)]);
stmt_to_str(#e_type_convert{expr = Expr, type = _Type}) ->
	io_lib:format("(~s as (~s))", [stmt_to_str(Expr), type_to_str_unimplemented]);
stmt_to_str(Any) ->
	Any.

special_char_map() ->
	#{$\n => "\\n", $\r => "\\r", $\t => "\\t", $\f => "\\f", $\b => "\\b"}.

-spec fix_special_chars(string()) -> string().
fix_special_chars(String) ->
	lists:map(fun(C) -> maps:get(C, special_char_map(), C) end, String).

-spec fmt(string(), [any()]) -> string().
fmt(FmtStr, Args) ->
	lists:flatten(io_lib:format(FmtStr, Args)).

-spec ethrow(location(), string()) -> no_return().
ethrow(Loc, FmtStr) ->
	ethrow(Loc, FmtStr, []).

-spec ethrow(location(), string(), [any()]) -> no_return().
%-define(NO_DEBUG, 1).
-ifndef(NO_DEBUG).
ethrow(Loc, FmtStr, Args) ->
	try
		throw({Loc, fmt(FmtStr, Args)})
	catch
		_:E:StackTrace ->
			io:format("stack trace: ~p~n", [StackTrace]),
			throw(E)
	end.
-else.
ethrow(Loc, FmtStr, Args) ->
	throw({Loc, fmt(FmtStr, Args)}).
-endif.

-spec get_values_by_keys([atom()], #{atom() => any()}) -> [any()].
get_values_by_keys(Fields, Map) ->
	lists:map(fun(K) -> maps:get(K, Map) end, Fields).

-spec get_kvpair_by_keys([atom()], #{atom() => any()}) -> [{atom(), any()}].
get_kvpair_by_keys(Names, Map) ->
	lists:zip(Names, get_values_by_keys(Names, Map)).

-ifdef(EUNIT).

get_values_by_keys_test() ->
	?assertEqual([3, 2], get_values_by_keys([a, b], #{c => 1, b => 2, a => 3})).

-endif.

-spec make_function_and_struct_map_from_ast(any()) -> {#{atom() => #e_fn_type{}}, #{atom() => #e_struct{}}}.
make_function_and_struct_map_from_ast(AST) ->
	{Fns, Structs} = lists:partition(fun(A) -> element(1, A) =:= e_function end, AST),
	%% FnTypeMap stores function type only
	FnTypeMap = maps:from_list(lists:map(fun(#e_function{name = Name} = Fn) -> {Name, Fn#e_function.type} end, Fns)),
	StructMap = maps:from_list(lists:map(fun(#e_struct{name = Name} = S) -> {Name, S} end, Structs)),
	{FnTypeMap, StructMap}.

%% address calculations
-spec fill_unit_pessi(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
fill_unit_pessi(Num, Unit) ->
	(Num + Unit - 1) div Unit * Unit.

-spec fill_unit_opti(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
fill_unit_opti(Num, Unit) ->
	(Num + Unit) div Unit * Unit.

-spec fall_unit(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
fall_unit(Num, Unit) ->
	Num div Unit * Unit.

%% The immediate value of `LUI` and `AUIPC` is a signed value.
%% When N is negative, the high part should be increased by 1 to balance it.
%% The mechanism is simple:
%% `+1` then `+(-1)` keeps the number unchanged. Negative signed extending can be treated as `-1`.
u_type_immedi(N) ->
	High = N bsr 12,
	Low = N band 16#FFF,
	case Low > 2047 of
		true ->
			{(High + 1) band 16#000FFFFF, fix_signed_num(Low, 12)};
		false ->
			{High band 16#000FFFFF, fix_signed_num(Low, 12)}
	end.

%% imm[20|10:1|11|19:12]
j_type_immedi(N) ->
	%% The lowest bit is dropped. bit-1 -> bit-0 and bit-20 -> bit-19
	N20 = (N bsr 20) bsl 19,
	N10_1 = ((N bsr 1) band 2#1111111111 bsl 9),
	N19_12 = (N bsr 12) band 2#11111111,
	N11 = ((N bsr 11) band 1 bsl 8),
	N20 bor N10_1 bor N11 bor N19_12.

%% High: imm[11:5]; Low: imm[4:0]
s_type_immedi(N) ->
	High = (N bsr 5) band 2#1111111,
	Low = N band 2#11111,
	{High, Low}.

%% High: imm[12|10:5]; Low: imm[4:1|11]
b_type_immedi(N) ->
	High = (((N bsr 12) band 1) bsl 7) bor ((N bsr 5) band 2#111111),
	Low = N band 2#11110 bor ((N bsr 11) band 1),
	{High, Low}.

fix_signed_num(N, BitNum) ->
	SignNum = sign_to_num((N bsr (BitNum - 1)) band 1),
	case SignNum of
		-1 ->
			SignNum * (((bnot N) + 1) band (bnot (-1 bsl BitNum)));
		1 ->
			N
	end.

sign_to_num(0) ->  1;
sign_to_num(1) -> -1.

-ifdef(EUNIT).

fix_signed_num_test() ->
	?assertEqual( 7, fix_signed_num(7, 4)),
	?assertEqual(-8, fix_signed_num(8, 4)),
	?assertEqual(-7, fix_signed_num(9, 4)).

u_type_immedi_test() ->
	?assertEqual({16#11223, fix_signed_num(16#344, 12)}, u_type_immedi(16#11223344)),
	?assertEqual({16#AABBD, fix_signed_num(16#CDD, 12)}, u_type_immedi(16#AABBCCDD)).

%% imm[20|10:1|11|19:12]
j_type_immedi_1_test() ->
	?assertEqual(2#0_0101010101_1_10101010, j_type_immedi(2#0_10101010_1_0101010101_0)).

j_type_immedi_2_test() ->
	N1 = 2#1_01010101_0_1010101010_1,
	N2 = 2#1_1010101010_0_01010101,
	?assertEqual(fix_signed_num(N2, 20), fix_signed_num(j_type_immedi(N1), 20)).

s_type_immedi_test() ->
	?assertEqual({2#1100110, 2#11101}, s_type_immedi(16#AABBCCDD)).

b_type_immedi_test() ->
	?assertEqual({2#0100110, 2#11101}, b_type_immedi(16#AABBCCDD)).

-endif.

-spec list_map(fun((E1, pos_integer()) -> E2), [E1]) -> [E2] when E1 :: any(), E2 :: any().
list_map(Fn, List) ->
	lists:map(fun({I, E}) -> Fn(E, I) end, lists:enumerate(0, List)).

void_type(Loc) ->
	#e_basic_type{class = void, tag = void, p_depth = 0, loc = Loc}.

-spec names_of_var_refs([#e_varref{}]) -> [atom()].
names_of_var_refs(VarRefList) ->
	lists:map(fun(#e_varref{name = Name}) -> Name end, VarRefList).

-spec names_of_var_defs([#e_vardef{}]) -> [atom()].
names_of_var_defs(VarDefList) ->
	lists:map(fun(#e_vardef{name = Name}) -> Name end, VarDefList).

-spec assert(boolean(), any()) -> ok.
assert(true, _) ->
	ok;
assert(false, Info) ->
	throw(Info).

-spec get_struct_from_type(#e_basic_type{}, #{atom() => #e_struct{}}) -> #e_struct{}.
get_struct_from_type(#e_basic_type{class = struct, tag = Name, loc = Loc}, StructMap) ->
	case maps:find(Name, StructMap) of
		{ok, S} ->
			S;
		error ->
			ethrow(Loc, "type \"~s\" is not found", [Name])
	end.

-spec get_struct_from_name(atom(), #{atom() => #e_struct{}}, location()) -> #e_struct{}.
get_struct_from_name(Name, StructMap, Loc) ->
	case maps:find(Name, StructMap) of
		{ok, S} ->
			S;
		error ->
			ethrow(Loc, "type \"~s\" is not found", [Name])
	end.


%% `merge_vars` will NOT merge all fields of e_vars. Only name, type_map and offset_map are merged.

-define(E_VARS(Names, TypeMap, OffsetMap), #e_vars{names = Names, type_map = TypeMap, offset_map = OffsetMap}).
-spec merge_vars(#e_vars{}, #e_vars{}, check_tag | ignore_tag) -> #e_vars{}.
merge_vars(#e_vars{tag = Tag1}, #e_vars{tag = Tag2}, check_tag) when Tag1 =/= Tag2 ->
	ethrow(0, "only vars with the same tag can be merged. (~s, ~s)", [Tag1, Tag2]);
merge_vars(?E_VARS(N1, M1, O1) = V, ?E_VARS(N2, M2, O2), _) ->
	V#e_vars{names = lists:append(N1, N2), type_map = maps:merge(M1, M2), offset_map = maps:merge(O1, O2)}.


-spec map_find_multi(any(), [#{any() => any()}]) -> {ok, _} | notfound.
map_find_multi(Key, [Map| RestMaps]) ->
	case maps:find(Key, Map) of
		{ok, _} = R ->
			R;
		error ->
			map_find_multi(Key, RestMaps)
	end;
map_find_multi(_, []) ->
	notfound.

-spec file_write(string(), fun((file:io_device()) -> R)) -> R when R :: ok.
file_write(Filename, Handle) ->
	{ok, IO_Dev} = file:open(Filename, [write]),
	try
		Handle(IO_Dev)
	after
		ok = file:close(IO_Dev)
	end.

