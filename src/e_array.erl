-module(e_array).
-export([transform_aref_in_ast/2, transform_aref_in_stmts/2]).
-include("e_record_definition.hrl").

-spec transform_aref_in_ast(e_ast(), e_compile_context:context()) -> e_ast().
transform_aref_in_ast([#e_function{vars = LocalVars, stmts = Stmts0} = Fn | Rest], #{vars := GlobalVars} = Ctx) ->
    Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
    Stmts1 = transform_aref_in_stmts(Stmts0, Ctx#{vars := Vars}),
    [Fn#e_function{stmts = Stmts1} | transform_aref_in_ast(Rest, Ctx)];
transform_aref_in_ast([Any | Rest], Ctx) ->
    [Any | transform_aref_in_ast(Rest, Ctx)];
transform_aref_in_ast([], _) ->
    [].

-spec transform_aref_in_stmts([e_stmt()], e_compile_context:context()) -> [e_stmt()].
transform_aref_in_stmts(Stmts0, Ctx) ->
    Stmts1 = e_util:expr_map(fun(E) -> transform_aref(E, Ctx) end, Stmts0),
    e_util:eliminate_pointer(Stmts1).

%% `a[b]` will be converted to `(a + b * sizeof(a^))^`.
-spec transform_aref(e_expr(), e_compile_context:context()) -> e_expr().
transform_aref(?AREF(Arr, Index, Loc), Ctx) ->
    ArrType = e_type:type_of_node(Arr, Ctx),
    Sizeof = #e_op{tag = {sizeof, e_type:inc_pointer_depth(ArrType, -1, Loc)}, loc = Loc},
    ?OP2('^', ?OP2('+', Arr, ?OP2('*', Index, Sizeof, Loc), Loc), ?I(0));
transform_aref(?CALL(Fn, Args) = Op, Ctx) ->
    Op?CALL(transform_aref(Fn, Ctx), lists:map(fun(E) -> transform_aref(E, Ctx) end, Args));
transform_aref(#e_op{data = Operands} = Op, Ctx) ->
    Op#e_op{data = lists:map(fun(E) -> transform_aref(E, Ctx) end, Operands)};
transform_aref(#e_type_convert{expr = Expr} = C, Ctx) ->
    C#e_type_convert{expr = transform_aref(Expr, Ctx)};
transform_aref(Any, _) ->
    Any.

