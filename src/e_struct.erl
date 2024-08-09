-module(e_struct).
-export([eliminate_dot/2]).
-include("e_record_definition.hrl").

-type context() :: e_struct_type_map().

-spec eliminate_dot(e_ast(), context()) -> e_ast().
eliminate_dot([#e_function{stmts = Stmts} = F | Rest], Context) ->
	[F#e_function{stmts = eliminate_dot_in_exprs(Stmts, Context)} | eliminate_dot(Rest, Context)];
eliminate_dot([Any | Rest], Context) ->
	[Any | eliminate_dot(Rest, Context)];
eliminate_dot([], _) ->
	[].

-spec eliminate_dot_in_exprs([e_stmt()], context()) -> [e_stmt()].
eliminate_dot_in_exprs(Stmts, Ctx) ->
	e_util:expr_map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Stmts).

-spec eliminate_dot_in_expr(e_expr(), context()) -> e_expr().
eliminate_dot_in_expr(#e_op{tag = '.', data = [O, #e_varref{name = VarName}]}, Ctx) ->
	ok;
eliminate_dot_in_expr(Any, _) ->
	Any.

