-module(e_pointer).
-export([fix_pointer_size_in_ast/3, fix_pointer_size_in_stmts/3]).
-include("./src/e_record_definition.hrl").

-spec fix_pointer_size_in_ast(e_ast(), e_type:context(), e_size:context()) -> e_ast().
fix_pointer_size_in_ast([#e_function{} = Fn | Rest], {GlobalVars, FnTypeMap, StructMap, _} = TypeCtx, SizeCtx) ->
	#e_function{vars = LocalVars, stmts = Stmts, type = FnType} = Fn,
	Vars = e_util:merge_vars(GlobalVars, LocalVars, ignore_tag),
	Ctx1 = {Vars, FnTypeMap, StructMap, FnType#e_fn_type.ret},
	[Fn#e_function{stmts = fix_pointer_size_in_stmts(Stmts, Ctx1, SizeCtx)} | fix_pointer_size_in_ast(Rest, TypeCtx, SizeCtx)];
fix_pointer_size_in_ast([Any | Rest], TypeCtx, SizeCtx) ->
	[Any | fix_pointer_size_in_ast(Rest, TypeCtx, SizeCtx)];
fix_pointer_size_in_ast([], _, _) ->
	[].

-spec fix_pointer_size_in_stmts([e_stmt()], e_type:context(), e_size:context()) -> [e_stmt()].
fix_pointer_size_in_stmts(Stmts0, TypeCtx, SizeCtx) ->
	e_util:expr_map(fun(E) -> fix_pointer_size(E, TypeCtx, SizeCtx) end, Stmts0).

-spec fix_pointer_size(e_expr(), e_type:context(), e_size:context()) -> e_expr().
fix_pointer_size(#e_op{tag = '^', data = [O, _], loc = Loc} = Op, TypeCtx, SizeCtx) ->
	T = e_type:dec_pointer_depth(e_type:type_of_node(O, TypeCtx), Loc),
	Op#e_op{data = [O, #e_integer{value = e_size:size_of(T, SizeCtx), loc = Loc}]};
fix_pointer_size(#e_op{tag = {call, Callee}, data = Args} = Op, TypeCtx, SizeCtx) ->
	Op#e_op{tag = {call, fix_pointer_size(Callee, TypeCtx, SizeCtx)}, data = lists:map(fun(E) -> fix_pointer_size(E, TypeCtx, SizeCtx) end, Args)};
fix_pointer_size(#e_op{data = Args} = Op, TypeCtx, SizeCtx) ->
	Op#e_op{data = lists:map(fun(E) -> fix_pointer_size(E, TypeCtx, SizeCtx) end, Args)};
fix_pointer_size(#e_type_convert{expr = Expr} = C, TypeCtx, SizeCtx) ->
	C#e_type_convert{expr = fix_pointer_size(Expr, TypeCtx, SizeCtx)};
fix_pointer_size(Any, _, _) ->
	Any.

