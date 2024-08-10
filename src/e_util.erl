-module(e_util).
-export([make_function_and_struct_map_from_ast/1, expr_map/2, merge_plus/1, assert/2]).
-export([stmt_to_str/1, filter_var_refs_in_map/2, get_values_by_keys/2, fmt/2, ethrow/3, ethrow/2]).
-export([names_of_var_defs/1, names_of_var_refs/1, value_in_list/2]).
-export([primitive_size_of/1, void_type/1, cut_extra/2, fill_unit_opti/2, fill_unit_pessi/2]).

-include("e_record_definition.hrl").

%% This function can be used to avoid boilerplate code for if, while, return, call...
%% So you can concentrate on operators.
-spec expr_map(fun((e_expr()) -> e_expr()), [e_stmt()]) -> [e_stmt()].
expr_map(Fn, [#e_if_stmt{} = If | Rest]) ->
	[If#e_if_stmt{condi = Fn(If#e_if_stmt.condi), then = expr_map(Fn, If#e_if_stmt.then), else = expr_map(Fn, If#e_if_stmt.else)} | expr_map(Fn, Rest)];
expr_map(Fn, [#e_while_stmt{condi = Cond, stmts = Stmts} = While | Rest]) ->
	[While#e_while_stmt{condi = Fn(Cond), stmts = expr_map(Fn, Stmts)} | expr_map(Fn, Rest)];
expr_map(Fn, [#e_return_stmt{expr = Expr} = Ret | Rest]) ->
	[Ret#e_return_stmt{expr = Fn(Expr)} | expr_map(Fn, Rest)];
expr_map(Fn, [#e_op{tag = {call, Callee}, data = Args} = FnCall | Rest]) ->
	[FnCall#e_op{tag = {call, Fn(Callee)}, data = expr_map(Fn, Args)} | expr_map(Fn, Rest)];
expr_map(Fn, [Any | Rest]) ->
	[Fn(Any) | expr_map(Fn, Rest)];
expr_map(_, []) ->
	[].

-define(PLUS_OP(O1, O2), #e_op{tag = '+', data = [O1, O2]}).

-spec merge_plus(e_expr()) -> e_expr().
merge_plus(?PLUS_OP(?PLUS_OP(O1, #e_integer{value = N1} = I), #e_integer{value = N2})) ->
	merge_plus(?PLUS_OP(O1, I#e_integer{value = N1 + N2}));
merge_plus(#e_op{data = Args} = Op) ->
	Op#e_op{data = e_util:expr_map(fun merge_plus/1, Args)};
merge_plus(Any) ->
	Any.

-spec stmt_to_str(e_stmt()) -> string().
stmt_to_str(#e_if_stmt{condi = Cond, then = Then, else = Else}) ->
	io_lib:format("if ~s then ~s else ~s end", [stmt_to_str(Cond), lists:map(fun stmt_to_str/1, Then), lists:map(fun stmt_to_str/1, Else)]);
stmt_to_str(#e_while_stmt{condi = Cond, stmts = Stmts}) ->
	io_lib:format("while ~s do ~s end", [stmt_to_str(Cond), lists:map(fun stmt_to_str/1, Stmts)]);
stmt_to_str(#e_return_stmt{expr = Expr}) ->
	io_lib:format("return (~s)", [stmt_to_str(Expr)]);
stmt_to_str(#e_varref{name = Name}) ->
	atom_to_list(Name);
stmt_to_str(#e_op{tag = {call, Callee}, data = Args}) ->
	io_lib:format("(~s)(~s)", [stmt_to_str(Callee), lists:map(fun stmt_to_str/1, Args)]);
stmt_to_str(#e_op{tag = Operator, data = [Op1, Op2]}) ->
	io_lib:format("~s ~s ~s", [stmt_to_str(Op1), Operator, stmt_to_str(Op2)]);
stmt_to_str(#e_op{tag = Operator, data = [Operand]}) ->
	io_lib:format("~s ~s", [stmt_to_str(Operand), Operator]);
stmt_to_str({TypeTag, _, Val}) when TypeTag =:= integer; TypeTag =:= float ->
	io_lib:format("~w", [Val]);
stmt_to_str({string, _, Val}) ->
	Val;
stmt_to_str(Any) ->
	Any.

-spec fmt(string(), [any()]) -> string().
fmt(FmtStr, Args) ->
	lists:flatten(io_lib:format(FmtStr, Args)).

-spec ethrow(non_neg_integer(), string()) -> no_return().
ethrow(Line, FmtStr) ->
	ethrow(Line, FmtStr, []).

-spec ethrow(non_neg_integer(), string(), [any()]) -> no_return().
ethrow(Line, FmtStr, Args) ->
	throw({Line, fmt(FmtStr, Args)}).

-spec get_values_by_keys([atom()], #{atom() => any()}) -> [any()].
get_values_by_keys(Fields, Map) when is_map(Map) ->
	get_values_by_keys(Fields, Map, []).

-spec get_values_by_keys([atom()], #{atom() => any()}, [any()]) -> [any()].
get_values_by_keys([Field | Rest], Map, Result) ->
	get_values_by_keys(Rest, Map, [maps:get(Field, Map) | Result]);
get_values_by_keys([], _, Result) ->
	lists:reverse(Result).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

get_values_by_keys_test() ->
	?assertEqual([3, 2], get_values_by_keys([a, b], #{c => 1, b => 2, a => 3})).

-endif.

-spec make_function_and_struct_map_from_ast(any()) -> {e_fn_type_map(), e_struct_type_map()}.
make_function_and_struct_map_from_ast(AST) ->
	{Fns, Structs} = lists:partition(fun(A) -> element(1, A) =:= e_function end, AST),
	%% FnTypeMap stores function type only
	FnTypeMap = maps:from_list(lists:map(fun(#e_function{name = Name} = Fn) -> {Name, Fn#e_function.type} end, Fns)),
	StructMap = maps:from_list(lists:map(fun(#e_struct{name = Name} = S) -> {Name, S} end, Structs)),
	{FnTypeMap, StructMap}.

%% address calculations
-spec fill_unit_pessi(integer(), non_neg_integer()) -> integer().
fill_unit_pessi(Num, Unit) ->
	(Num + Unit - 1) div Unit * Unit.

-spec fill_unit_opti(integer(), non_neg_integer()) -> integer().
fill_unit_opti(Num, Unit) ->
	(Num + Unit) div Unit * Unit.

-spec cut_extra(integer(), non_neg_integer()) -> integer().
cut_extra(Num, Unit) ->
	Num div Unit * Unit.

-spec primitive_size_of(atom()) -> pointer_size | 1 | 2 | 4 | 8.
primitive_size_of(usize) -> pointer_size;
primitive_size_of(isize) -> pointer_size;
primitive_size_of(uptr) -> pointer_size;
primitive_size_of(iptr) -> pointer_size;
primitive_size_of(byte) -> 1;
primitive_size_of(u64) -> 8;
primitive_size_of(i64) -> 8;
primitive_size_of(u32) -> 4;
primitive_size_of(i32) -> 4;
primitive_size_of(u16) -> 2;
primitive_size_of(i16) -> 2;
primitive_size_of(u8) -> 1;
primitive_size_of(i8) -> 1;
primitive_size_of(f64) -> 8;
primitive_size_of(f32) -> 4;
primitive_size_of(T) -> throw(fmt("size of ~p is not defined", [T])).

void_type(Line) ->
	#e_basic_type{class = void, tag = void, p_depth = 0, line = Line}.

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

-spec value_in_list(any(), [any()]) -> boolean().
value_in_list(Value, List) ->
	lists:any(fun(V) -> V =:= Value end, List).

%% filter_var_refs_in_map([#e_varref{name = a}, #e_varref{name = b}], #{a => 1})
%% > [#e_varref{name = a}].
-spec filter_var_refs_in_map([#e_varref{}], #{atom() := any()}) -> [#e_varref{}].
filter_var_refs_in_map(VarRefList, TargetMap) ->
	lists:filter(fun(#e_varref{name = Name}) -> exist_in_map(Name, TargetMap) end, VarRefList).

-ifdef(EUNIT).

filter_var_refs_in_map_test() ->
	A = filter_var_refs_in_map([#e_varref{name = a}, #e_varref{name = b}], #{a => 1}),
	?assertEqual(A, [#e_varref{name = a}]).

-endif.

-spec exist_in_map(atom(), #{atom() := any()}) -> boolean().
exist_in_map(KeyName, Map) ->
	case maps:find(KeyName, Map) of
		{ok, _} ->
			true;
		_ ->
			false
	end.

