-module(e_struct).
-export([eliminate_dot_in_ast/3, eliminate_dot_in_stmts/3]).
-include("e_record_definition.hrl").

%% This `context()` is the same as the one in `e_type.erl`.
-type context() :: {e_var_type_map(), e_fn_type_map(), e_struct_type_map(), e_type()}.

-spec eliminate_dot_in_ast(e_ast(), e_var_type_map(), {e_fn_type_map(), e_struct_type_map()}) -> e_ast().
eliminate_dot_in_ast([#e_function{stmts = Stmts0} = Fn | Rest], GlobalVarTypes, {FnTypeMap, StructMap} = Maps) ->
	CurrentVars = maps:merge(GlobalVarTypes, Fn#e_function.var_type_map),
	Ctx = {CurrentVars, FnTypeMap, StructMap, #e_basic_type{}},
	Stmts1 = eliminate_dot(Stmts0, Ctx),
	[Fn#e_function{stmts = Stmts1} | eliminate_dot_in_ast(Rest, GlobalVarTypes, Maps)];
eliminate_dot_in_ast([Any | Rest], GlobalVarTypes, Ctx) ->
	[Any | eliminate_dot_in_ast(Rest, GlobalVarTypes, Ctx)];
eliminate_dot_in_ast([], _, _) ->
	[].

-spec eliminate_dot_in_stmts([e_stmt()], e_var_type_map(), {e_fn_type_map(), e_struct_type_map()}) -> [e_stmt()].
eliminate_dot_in_stmts(Stmts, GlobalVarTypes, {FnTypeMap, StructMap}) ->
	Ctx = {GlobalVarTypes, FnTypeMap, StructMap, #e_basic_type{}},
	eliminate_dot(Stmts, Ctx).

-spec eliminate_dot([e_stmt()], context()) -> [e_stmt()].
eliminate_dot(Stmts0, Ctx) ->
	Stmts1 = e_util:expr_map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Stmts0),
	Stmts2 = e_util:expr_map(fun eliminate_pointer_ops/1, Stmts1),
	Stmts3 = e_util:expr_map(fun e_util:merge_plus/1, Stmts2),
	Stmts3.

%% `a.b` will be converted to `(a@ + OFFSET_OF_b)^`.
-spec eliminate_dot_in_expr(e_expr(), context()) -> e_expr().
eliminate_dot_in_expr(#e_op{tag = '.', line = L} = Op, {_, _, StructMap, _} = Ctx) ->
	#e_op{data = [O, #e_varref{name = FieldName}]} = Op,
	#e_basic_type{class = struct, tag = Name, p_depth = 0} = e_type:type_of_node(O, Ctx),
	{ok, #e_struct{field_offset_map = FieldOffsetMap}} = maps:find(Name, StructMap),
	{ok, Offset} = maps:find(FieldName, FieldOffsetMap),
	A = #e_op{tag = '@', data = [eliminate_dot_in_expr(O, Ctx)], line = L},
	B = #e_op{tag = '+', data = [A, #e_integer{value = Offset, line = L}], line = L},
	#e_op{tag = '^', data = [B]};
eliminate_dot_in_expr(#e_op{data = Args} = Op, Ctx) ->
	Op#e_op{data = e_util:expr_map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Args)};
eliminate_dot_in_expr(Any, _) ->
	Any.

-spec eliminate_pointer_ops(e_expr()) -> e_expr().
eliminate_pointer_ops(#e_op{tag = '^', data = [#e_op{tag = '@', data = [E]}]}) ->
	eliminate_pointer_ops(E);
eliminate_pointer_ops(#e_op{tag = '@', data = [#e_op{tag = '^', data = [E]}]}) ->
	eliminate_pointer_ops(E);
eliminate_pointer_ops(#e_op{data = Args} = Op) ->
	Op#e_op{data = e_util:expr_map(fun eliminate_pointer_ops/1, Args)};
eliminate_pointer_ops(Any) ->
	Any.

