-module(e_expr).
-export([decompose_expr_in_ast/2, decompose_expr_in_stmts/2]).
-include("./e_record_definition.hrl").

-spec decompose_expr_in_ast(e_ast(), _) -> e_ast().
decompose_expr_in_ast(AST, _) ->
	AST.

-spec decompose_expr_in_stmts([e_stmt()], _) -> [e_stmt()].
decompose_expr_in_stmts(Stmts, Ctx) ->
	e_util:expr_map(fun(E) -> decompose_expr_in_expr(E, Ctx) end, Stmts).

decompose_expr_in_expr(Any, _) ->
	Any.

