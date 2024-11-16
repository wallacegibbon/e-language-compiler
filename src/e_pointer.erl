-module(e_pointer).
-export([fix_pointer_size_in_ast/2, fix_pointer_size_in_stmts/2]).
-include("./src/e_record_definition.hrl").

-spec fix_pointer_size_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
fix_pointer_size_in_ast([#e_function{vars = LocalVars, stmts = Stmts} = Fn | Rest], #{vars := GlobalVars} = Ctx) ->
    Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
    Ctx1 = Ctx#{vars := Vars},
    [Fn#e_function{stmts = fix_pointer_size_in_stmts(Stmts, Ctx1)} | fix_pointer_size_in_ast(Rest, Ctx)];
fix_pointer_size_in_ast([Any | Rest], Ctx) ->
    [Any | fix_pointer_size_in_ast(Rest, Ctx)];
fix_pointer_size_in_ast([], _) ->
    [].

-spec fix_pointer_size_in_stmts([e_stmt()], e_compile_context:context()) -> [e_stmt()].
fix_pointer_size_in_stmts(Stmts0, Ctx) ->
    e_util:expr_map(fun(E) -> fix_pointer_size(E, Ctx) end, Stmts0).

-spec fix_pointer_size(e_expr(), e_compile_context:context()) -> e_expr().
fix_pointer_size(?OP2('^', O, ?I(0), Loc) = Op, Ctx) ->
    T = e_type:inc_pointer_depth(e_type:type_of_node(O, Ctx), -1, Loc),
    Op?OP2('^', O, ?I(e_size:size_of(T, Ctx), Loc));
fix_pointer_size(?CALL(Fn, Args) = Op, Ctx) ->
    Op?CALL(fix_pointer_size(Fn, Ctx), lists:map(fun(E) -> fix_pointer_size(E, Ctx) end, Args));
fix_pointer_size(#e_op{data = Operands} = Op, Ctx) ->
    Op#e_op{data = lists:map(fun(E) -> fix_pointer_size(E, Ctx) end, Operands)};
fix_pointer_size(#e_type_convert{expr = Expr} = C, Ctx) ->
    C#e_type_convert{expr = fix_pointer_size(Expr, Ctx)};
fix_pointer_size(Any, _) ->
    Any.

