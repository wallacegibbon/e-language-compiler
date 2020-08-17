-module(ecompiler_utils).

-export([exprsmap/2, flat_format/2]).

-include("./ecompiler_frame.hrl").

exprsmap(Fn, [#if_expr{condition=Condition, then=Then, else=Else} = If |
	      Rest]) ->
    [If#if_expr{condition=Fn(Condition), then=exprsmap(Fn, Then),
		else=exprsmap(Fn, Else)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [#while_expr{condition=Condition, exprs=Exprs} = While |
	      Rest]) ->
    [While#while_expr{condition=Fn(Condition), exprs=exprsmap(Fn, Exprs)} |
     exprsmap(Fn, Rest)];
%% @TODO:
%exprsmap(Fn, [#call{fn=Callee, args=Args} = Fncall | Rest]) ->
%    [Fncall#call{fn=Fn(Callee), args=exprsmap(Fn, Args)} |
exprsmap(Fn, [#call{args=Args} = Fncall | Rest]) ->
    [Fncall#call{args=exprsmap(Fn, Args)} |
     exprsmap(Fn, Rest)];
exprsmap(Fn, [#return{expr=Retexpr} = Return | Rest]) ->
    [Return#return{expr=Fn(Retexpr)} | exprsmap(Fn, Rest)];
exprsmap(Fn, [Any | Rest]) ->
    [Fn(Any) | exprsmap(Fn, Rest)];
exprsmap(_, []) ->
    [].

flat_format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).

