-module(e_init_expr).

-export([expand_in_function/2, expand_init_expr/2]).

-include("e_record_definition.hrl").

%% for now, array and struct init expression is only allowed in assignment
check_position(#e_expr{tag = '=', data = [_, Op2]})
	when is_record(Op2, struct_init_expr); is_record(Op2, array_init_expr) ->
	ok;
check_position(#struct_init_expr{line = Line}) ->
	e_util:ethrow(Line, "struct init expression is only allowed in assignments");
check_position(#array_init_expr{line = Line}) ->
	e_util:ethrow(Line, "array init expression is only allowed in assignments");
check_position(_) ->
	ok.

-spec expand_in_function(e_ast(), struct_type_map()) -> e_ast().
expand_in_function([#function{stmts = Exprs} = F | Rest], StructMap) ->
	e_util:expr_map(fun check_position/1, Exprs),
	[F#function{stmts = expand_init_expr(Exprs, StructMap)} | expand_in_function(Rest, StructMap)];
expand_in_function([Any | Rest], StructMap) ->
	[Any | expand_in_function(Rest, StructMap)];
expand_in_function([], _) ->
	[].

expand_init_expr(Exprs, StructMap) ->
	expand_init_expr(Exprs, [], StructMap).

expand_init_expr([#if_stmt{then = Then, else = Else} = E | Rest], NewAST, StructMap) ->
	NewE = E#if_stmt{then = expand_init_expr(Then, [], StructMap), else = expand_init_expr(Else, [], StructMap)},
	expand_init_expr(Rest, [NewE | NewAST], StructMap);
expand_init_expr([#while_stmt{stmts = Exprs} = E | Rest], NewAST, StructMap) ->
	NewE = E#while_stmt{stmts = expand_init_expr(Exprs, [], StructMap)},
	expand_init_expr(Rest, [NewE | NewAST], StructMap);
expand_init_expr([#e_expr{data = [_, _]} = Op | Rest], NewAST, StructMap) ->
	expand_init_expr(Rest, replace_init_ops(Op, StructMap) ++ NewAST, StructMap);
expand_init_expr([Any | Rest], NewAST, StructMap) ->
	expand_init_expr(Rest, [Any | NewAST], StructMap);
expand_init_expr([], NewAST, _) ->
	lists:reverse(NewAST).

replace_init_ops(#e_expr{tag = '=', data = [Op1, #struct_init_expr{} = D]}, StructMap) ->
	#struct_init_expr{name = Name, line = Line, field_value_map = FieldValues} = D,
	case maps:find(Name, StructMap) of
		{ok, #struct{field_names = FieldNames, field_type_map = FieldTypes, field_default_value_map = FieldDefaults}} ->
			FieldValueMap = maps:merge(FieldDefaults, FieldValues),
			struct_init_to_ops(Op1, FieldNames, FieldValueMap, FieldTypes, [], StructMap);
		error ->
			e_util:ethrow(Line, "struct ~s is not found", [Name])
	end;
replace_init_ops(#e_expr{tag = '=', data = [Op1, #array_init_expr{} = D]}, StructMap) ->
	#array_init_expr{elements = Elements, line = Line} = D,
	array_init_to_ops(Op1, Elements, 0, Line, [], StructMap);
replace_init_ops(Any, _) ->
	[Any].

struct_init_to_ops(Target, VarRefs, FieldInitMap, FieldTypes, NewCode, StructMap) ->
	[#var_ref{line = Line, name = Name} = Field | Rest] = VarRefs,
	RValue =
		case maps:find(Name, FieldInitMap) of
			{ok, InitOp} ->
				InitOp;
			error ->
				default_value_of(maps:get(Name, FieldTypes), Line)
		end,
	NewData = [#e_expr{tag = '.', data = [Target, Field], line = Line}, RValue],
	NewOp = #e_expr{tag = '=', data = NewData, line = Line},
	Ops = replace_init_ops(NewOp, StructMap),
	struct_init_to_ops(Target, Rest, FieldInitMap, FieldTypes, Ops ++ NewCode, StructMap);
struct_init_to_ops(_, [], _, _, NewCode, _) ->
	NewCode.

default_value_of(#array_type{elem_type = Type, length = Len}, Line) ->
	#array_init_expr{elements = lists:duplicate(Len, default_value_of(Type, Line)), line = Line};
default_value_of(#basic_type{class = struct, tag = Tag, p_depth = 0}, Line) ->
	#struct_init_expr{name = Tag, line = Line, field_value_map = #{}, field_names = []};
default_value_of(#basic_type{class = integer, p_depth = 0}, Line) ->
	{integer, Line, 0};
default_value_of(#basic_type{class = float, p_depth = 0}, Line) ->
	{float, Line, 0.0};
default_value_of(#basic_type{p_depth = PDepth}, Line) when PDepth > 0 ->
	{integer, Line, 0}.

array_init_to_ops(Target, [E | Rest], Cnt, Line, NewCode, StructMap) ->
	A = [#e_expr{tag = '@', data = [Target], line = Line}, #integer{value = Cnt, line = Line}],
	B = [#e_expr{tag = '+', data = A, line = Line}], C = #e_expr{tag = '^', data = B, line = Line},
	NewAssign = #e_expr{tag = '=', data = [C, E], line = Line},
	Ops = replace_init_ops(NewAssign, StructMap),
	array_init_to_ops(Target, Rest, Cnt + 1, Line, Ops ++ NewCode, StructMap);
array_init_to_ops(_, [], _, _, NewCode, _) ->
	NewCode.

