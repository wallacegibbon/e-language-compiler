-module(e_init_expr).
-export([expand_in_ast/2, expand_in_stmts/2]).
-include("e_record_definition.hrl").

-type context() :: {#{atom() => #e_struct{}}, non_neg_integer()}.

-spec expand_in_ast(e_ast(), context()) -> e_ast().
expand_in_ast([#e_function{stmts = Stmts} = Fn | Rest], Ctx) ->
	e_util:expr_map(fun check_position/1, Stmts),
	[Fn#e_function{stmts = expand_in_stmts(Stmts, Ctx)} | expand_in_ast(Rest, Ctx)];
expand_in_ast([Any | Rest], Ctx) ->
	[Any | expand_in_ast(Rest, Ctx)];
expand_in_ast([], _) ->
	[].

expand_in_stmts(Stmts, Ctx) ->
	expand(Stmts, [], Ctx).

expand([#e_if_stmt{then = Then, 'else' = Else} = E | Rest], NewAST, Ctx) ->
	expand(Rest, [E#e_if_stmt{then = expand(Then, [], Ctx), 'else' = expand(Else, [], Ctx)} | NewAST], Ctx);
expand([#e_while_stmt{stmts = Stmts} = E | Rest], NewAST, Ctx) ->
	expand(Rest, [E#e_while_stmt{stmts = expand(Stmts, [], Ctx)} | NewAST], Ctx);
expand([#e_op{data = [_, _]} = Op | Rest], NewAST, Ctx) ->
	expand(Rest, replace_init_ops(Op, Ctx) ++ NewAST, Ctx);
expand([Any | Rest], NewAST, Ctx) ->
	expand(Rest, [Any | NewAST], Ctx);
expand([], NewAST, _) ->
	lists:reverse(NewAST).

%% for now, array and struct init expression is only allowed in assignment
check_position(#e_op{tag = '=', data = [_, #e_struct_init_expr{}]}) ->
	ok;
check_position(#e_op{tag = '=', data = [_, #e_array_init_expr{}]}) ->
	ok;
check_position(#e_op{tag = {call, Callee}, data = Data}) ->
	check_position(Callee),
	lists:foreach(fun check_position/1, Data);
check_position(#e_op{data = Data}) ->
	lists:foreach(fun check_position/1, Data);
check_position(#e_type_convert{expr = Expr}) ->
	check_position(Expr);
check_position(#e_struct_init_expr{loc = Loc}) ->
	e_util:ethrow(Loc, "struct init expression is only allowed in assignments");
check_position(#e_array_init_expr{loc = Loc}) ->
	e_util:ethrow(Loc, "array init expression is only allowed in assignments");
check_position(_) ->
	ok.

replace_init_ops(#e_op{tag = '=', data = [Op1, #e_struct_init_expr{} = D]}, {StructMap, _} = Ctx) ->
	#e_struct_init_expr{name = Name, loc = Loc, field_value_map = FieldValues} = D,
	Struct = e_util:get_struct_from_name(Name, StructMap, Loc),
	#e_struct{fields = Fields, default_value_map = FieldDefaultMap} = Struct,
	#e_vars{names = FieldNames, type_map = FieldTypeMap} = Fields,
	VarRefs = lists:map(fun(N) -> #e_varref{name = N, loc = Loc} end, FieldNames),
	FieldInitMap = maps:merge(FieldDefaultMap, FieldValues),
	struct_init_to_ops(Op1, VarRefs, FieldInitMap, FieldTypeMap, [], Ctx);
replace_init_ops(#e_op{tag = '=', data = [Op1, #e_array_init_expr{} = D]}, Ctx) ->
	#e_array_init_expr{elements = Elements, loc = Loc} = D,
	array_init_to_ops(Op1, Elements, 0, Loc, [], Ctx);
replace_init_ops(Any, _) ->
	[Any].

struct_init_to_ops(Target, [#e_varref{} | _] = VarRefs, FieldInitMap, FieldTypeMap, NewCode, Ctx) ->
	[#e_varref{name = Name, loc = Loc} = Field | Rest] = VarRefs,
	RValue = maps:get(Name, FieldInitMap, default_value_of(maps:get(Name, FieldTypeMap), Loc)),
	NewData = [#e_op{tag = '.', data = [Target, Field], loc = Loc}, RValue],
	NewOp = #e_op{tag = '=', data = NewData, loc = Loc},
	Ops = replace_init_ops(NewOp, Ctx),
	struct_init_to_ops(Target, Rest, FieldInitMap, FieldTypeMap, Ops ++ NewCode, Ctx);
struct_init_to_ops(_, [], _, _, NewCode, _) ->
	NewCode.

default_value_of(#e_array_type{elem_type = Type, length = Len}, Loc) ->
	#e_array_init_expr{elements = lists:duplicate(Len, default_value_of(Type, Loc)), loc = Loc};
default_value_of(#e_basic_type{class = struct, tag = Tag, p_depth = 0}, Loc) ->
	#e_struct_init_expr{name = Tag, loc = Loc, field_value_map = #{}};
default_value_of(#e_basic_type{class = integer, p_depth = 0}, Loc) ->
	#e_integer{value = 0, loc = Loc};
default_value_of(#e_basic_type{class = float, p_depth = 0}, Loc) ->
	#e_float{value = 0.0, loc = Loc};
default_value_of(#e_basic_type{p_depth = PDepth}, Loc) when PDepth > 0 ->
	#e_integer{value = 0, loc = Loc}.

array_init_to_ops(Target, [E | Rest], Cnt, Loc, NewCode, {_, WordSize} = Ctx) ->
	A = [#e_op{tag = '@', data = [Target], loc = Loc}, #e_integer{value = Cnt * WordSize, loc = Loc}],
	B = [#e_op{tag = '+', data = A, loc = Loc}, #e_integer{value = 0, loc = Loc}],
	C = #e_op{tag = '^', data = B, loc = Loc},
	NewAssign = #e_op{tag = '=', data = [C, E], loc = Loc},
	Ops = replace_init_ops(NewAssign, Ctx),
	array_init_to_ops(Target, Rest, Cnt + 1, Loc, Ops ++ NewCode, Ctx);
array_init_to_ops(_, [], _, _, NewCode, _) ->
	NewCode.

