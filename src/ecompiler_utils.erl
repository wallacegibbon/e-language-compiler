-module(ecompiler_utils).

-export([exprsmap/2, expr2str/1, flat_format/2, is_primitive_type/1,
	 is_integer_type/1, getvalues_bykeys/2, void_type/1, any_type/1]).

-include("./ecompiler_frame.hrl").

%% when do simple convertions, this function can be used to avoid boilerplate
%% code for if, while, return, call...,  so you can concentrate on op1, op2...
exprsmap(Fn, [#if_expr{condition=Cond, then=Then, else=Else} = If | Rest]) ->
    [If#if_expr{condition=Fn(Cond), then=exprsmap(Fn, Then),
		else=exprsmap(Fn, Else)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [#while_expr{condition=Cond, exprs=Exprs} = While | Rest]) ->
    [While#while_expr{condition=Fn(Cond), exprs=exprsmap(Fn, Exprs)} |
     exprsmap(Fn, Rest)];
exprsmap(Fn, [#call{fn=Callee, args=Args} = Fncall | Rest]) ->
    [Fncall#call{fn=Fn(Callee), args=exprsmap(Fn, Args)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [#return{expr=Retexpr} = Return | Rest]) ->
    [Return#return{expr=Fn(Retexpr)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [Any | Rest]) ->
    [Fn(Any) | exprsmap(Fn, Rest)];
exprsmap(_, []) ->
    [].

expr2str(#if_expr{condition=Cond, then=Then, else=Else}) ->
    io_lib:format("if (~s) ~s else ~s end", [expr2str(Cond), expr2str(Then),
					     expr2str(Else)]);
expr2str(#while_expr{condition=Cond, exprs=Exprs}) ->
    io_lib:format("while (~s) ~s end", [expr2str(Cond), expr2str(Exprs)]);
expr2str(#call{fn=Callee, args=Args}) ->
    io_lib:format("(~s)(~s)", [expr2str(Callee), expr2str(Args)]);
expr2str(#return{expr=Retexpr}) ->
    io_lib:format("return (~s)", [expr2str(Retexpr)]);
expr2str(#varref{name=Name}) ->
    io_lib:format("~s", [expr2str(Name)]);
expr2str(#op2{operator=Operator, op1=Op1, op2=Op2}) ->
    io_lib:format("~s ~s ~s", [expr2str(Op1), Operator, expr2str(Op2)]);
expr2str(#op1{operator=Operator, operand=Operand}) ->
    io_lib:format("~s ~s", [expr2str(Operand), Operator]);
expr2str({Immi, _, Val}) when Immi =:= integer; Immi =:= float ->
    io_lib:format("~w", [Val]);
expr2str({Immi, _, Val}) when Immi =:= string ->
    Val;
expr2str(Any) ->
    Any.

flat_format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).

-spec getvalues_bykeys([atom()], #{atom() => any()}) -> [any()].
getvalues_bykeys(Fields, Map) when is_map(Map) ->
    getvalues_bykeys(Fields, Map, []).

getvalues_bykeys([Field | Rest], Map, Result) ->
    getvalues_bykeys(Rest, Map, [maps:get(Field, Map) | Result]);
getvalues_bykeys([], _, Result) ->
    lists:reverse(Result).

is_primitive_type(void) -> true;
is_primitive_type(any) -> true;
is_primitive_type(T) -> is_integer_type(T).

is_integer_type(f64) -> true;
is_integer_type(f32) -> true;
is_integer_type(u64) -> true;
is_integer_type(u32) -> true;
is_integer_type(u16) -> true;
is_integer_type(u8) -> true;
is_integer_type(i64) -> true;
is_integer_type(i32) -> true;
is_integer_type(i16) -> true;
is_integer_type(i8) -> true;
is_integer_type(_) -> false.

void_type(Line) ->
    #basic_type{type={void, 0}, line=Line}.

any_type(Line) ->
    #basic_type{type={any, 0}, line=Line}.

