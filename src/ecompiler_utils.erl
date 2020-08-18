-module(ecompiler_utils).

-export([exprsmap/2, expr2str/1, flat_format/2]).

-include("./ecompiler_frame.hrl").

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
    io_lib:format("(~s~s)", [expr2str(Operand), Operator]);
expr2str({Immi, _, Val}) when Immi =:= integer; Immi =:= float ->
    io_lib:format("~w", [Val]);
expr2str({Immi, _, Val}) when Immi =:= string ->
    Val;
expr2str(Any) ->
    Any.

flat_format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).

