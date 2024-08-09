-module(e_struct).
-export([eliminate_dot/3]).
-include("e_record_definition.hrl").

%% This `context()` is the same as the one in `e_type.erl`.
-type context() :: {e_var_type_map(), e_fn_type_map(), e_struct_type_map(), e_type()}.

-spec eliminate_dot(e_ast(), e_var_type_map(), {e_fn_type_map(), e_struct_type_map()}) -> e_ast().
eliminate_dot([#e_function{stmts = Stmts} = Fn | Rest], GlobalVarTypes, {FnTypeMap, StructMap} = Maps) ->
	CurrentVars = maps:merge(GlobalVarTypes, Fn#e_function.e_var_type_map),
	Ctx = {CurrentVars, FnTypeMap, StructMap, #e_basic_type{}},
	[Fn#e_function{stmts = eliminate_dot_in_exprs(Stmts, Ctx)} | eliminate_dot(Rest, GlobalVarTypes, Maps)];
eliminate_dot([Any | Rest], GlobalVarTypes, Ctx) ->
	[Any | eliminate_dot(Rest, GlobalVarTypes, Ctx)];
eliminate_dot([], _, _) ->
	[].

-spec eliminate_dot_in_exprs([e_stmt()], context()) -> [e_stmt()].
eliminate_dot_in_exprs(Stmts, Ctx) ->
	e_util:expr_map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Stmts).

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
	Op#e_op{data = eliminate_dot_in_exprs(Args, Ctx)};
eliminate_dot_in_expr(Any, _) ->
	Any.

