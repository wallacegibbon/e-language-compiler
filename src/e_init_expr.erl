-module(e_init_expr).
-export([expand_in_ast/2, expand_in_stmts/2]).
-include("e_record_definition.hrl").

%% for now, array and struct init expression is only allowed in assignment
check_position(#e_op{tag = '=', data = [_, #e_struct_init_expr{}]}) ->
	ok;
check_position(#e_op{tag = '=', data = [_, #e_array_init_expr{}]}) ->
	ok;
check_position(#e_op{data = Data}) ->
	lists:foreach(fun check_position/1, Data);
check_position(#e_struct_init_expr{line = Line}) ->
	e_util:ethrow(Line, "struct init expression is only allowed in assignments");
check_position(#e_array_init_expr{line = Line}) ->
	e_util:ethrow(Line, "array init expression is only allowed in assignments");
check_position(_) ->
	ok.

-spec expand_in_ast(e_ast(), #{atom() => #e_struct{}}) -> e_ast().
expand_in_ast([#e_function{stmts = Stmts} = Fn | Rest], StructMap) ->
	e_util:expr_map(fun check_position/1, Stmts),
	[Fn#e_function{stmts = expand_in_stmts(Stmts, StructMap)} | expand_in_ast(Rest, StructMap)];
expand_in_ast([Any | Rest], StructMap) ->
	[Any | expand_in_ast(Rest, StructMap)];
expand_in_ast([], _) ->
	[].

expand_in_stmts(Stmts, StructMap) ->
	expand(Stmts, [], StructMap).

expand([#e_if_stmt{then = Then, 'else' = Else} = E | Rest], NewAST, StructMap) ->
	expand(Rest, [E#e_if_stmt{then = expand(Then, [], StructMap), 'else' = expand(Else, [], StructMap)} | NewAST], StructMap);
expand([#e_while_stmt{stmts = Stmts} = E | Rest], NewAST, StructMap) ->
	expand(Rest, [E#e_while_stmt{stmts = expand(Stmts, [], StructMap)} | NewAST], StructMap);
expand([#e_op{data = [_, _]} = Op | Rest], NewAST, StructMap) ->
	expand(Rest, replace_init_ops(Op, StructMap) ++ NewAST, StructMap);
expand([Any | Rest], NewAST, StructMap) ->
	expand(Rest, [Any | NewAST], StructMap);
expand([], NewAST, _) ->
	lists:reverse(NewAST).

replace_init_ops(#e_op{tag = '=', data = [Op1, #e_struct_init_expr{} = D]}, StructMap) ->
	#e_struct_init_expr{name = Name, line = Line, field_value_map = FieldValues} = D,
	Struct = e_util:get_struct_from_name(Name, StructMap, Line),
	#e_struct{fields = Fields, default_value_map = FieldDefaultMap} = Struct,
	#e_vars{names = FieldNames, type_map = FieldTypeMap} = Fields,
	VarRefs = lists:map(fun(N) -> #e_varref{line = Line, name = N} end, FieldNames),
	FieldInitMap = maps:merge(FieldDefaultMap, FieldValues),
	struct_init_to_ops(Op1, VarRefs, FieldInitMap, FieldTypeMap, [], StructMap);
replace_init_ops(#e_op{tag = '=', data = [Op1, #e_array_init_expr{} = D]}, StructMap) ->
	#e_array_init_expr{elements = Elements, line = Line} = D,
	array_init_to_ops(Op1, Elements, 0, Line, [], StructMap);
replace_init_ops(Any, _) ->
	[Any].

struct_init_to_ops(Target, [#e_varref{} | _] = VarRefs, FieldInitMap, FieldTypeMap, NewCode, StructMap) ->
	[#e_varref{line = Line, name = Name} = Field | Rest] = VarRefs,
	RValue = maps:get(Name, FieldInitMap, default_value_of(maps:get(Name, FieldTypeMap), Line)),
	NewData = [#e_op{tag = '.', data = [Target, Field], line = Line}, RValue],
	NewOp = #e_op{tag = '=', data = NewData, line = Line},
	Ops = replace_init_ops(NewOp, StructMap),
	struct_init_to_ops(Target, Rest, FieldInitMap, FieldTypeMap, Ops ++ NewCode, StructMap);
struct_init_to_ops(_, [], _, _, NewCode, _) ->
	NewCode.

default_value_of(#e_array_type{elem_type = Type, length = Len}, Line) ->
	#e_array_init_expr{elements = lists:duplicate(Len, default_value_of(Type, Line)), line = Line};
default_value_of(#e_basic_type{class = struct, tag = Tag, p_depth = 0}, Line) ->
	#e_struct_init_expr{name = Tag, line = Line, field_value_map = #{}};
default_value_of(#e_basic_type{class = integer, p_depth = 0}, Line) ->
	#e_integer{line = Line, value = 0};
default_value_of(#e_basic_type{class = float, p_depth = 0}, Line) ->
	#e_float{line = Line, value = 0.0};
default_value_of(#e_basic_type{p_depth = PDepth}, Line) when PDepth > 0 ->
	#e_integer{line = Line, value = 0}.

array_init_to_ops(Target, [E | Rest], Cnt, Line, NewCode, StructMap) ->
	A = [#e_op{tag = '@', data = [Target], line = Line}, #e_integer{value = Cnt, line = Line}],
	B = [#e_op{tag = '+', data = A, line = Line}], C = #e_op{tag = '^', data = B, line = Line},
	NewAssign = #e_op{tag = '=', data = [C, E], line = Line},
	Ops = replace_init_ops(NewAssign, StructMap),
	array_init_to_ops(Target, Rest, Cnt + 1, Line, Ops ++ NewCode, StructMap);
array_init_to_ops(_, [], _, _, NewCode, _) ->
	NewCode.

