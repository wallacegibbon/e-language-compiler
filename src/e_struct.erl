-module(e_struct).
-export([check_struct_recursion/1, eliminate_dot_in_ast/3, eliminate_dot_in_stmts/3]).
-include("e_record_definition.hrl").

-spec check_struct_recursion(#{atom() => #e_struct{}}) -> ok.
check_struct_recursion(StructTypeMap) ->
	maps:foreach(fun(_, S) -> check_struct_recursion_one(S, StructTypeMap) end, StructTypeMap).

-spec check_struct_recursion_one(#e_struct{}, #{atom() => #e_struct{}}) -> ok.
check_struct_recursion_one(#e_struct{name = Name, loc = Loc} = Struct, StructTypeMap) ->
	try
		check_struct_object(Struct, StructTypeMap, [])
	catch
		{recur, Chain} ->
			Str = string:join(lists:map(fun atom_to_list/1, Chain), "/"),
			e_util:ethrow(Loc, "recursion in struct ~s: ~s", [Name, Str])
	end.

-spec check_struct_object(#e_struct{}, #{atom() => #e_struct{}}, [atom()]) -> ok | {recur, [any()]}.
check_struct_object(#e_struct{name = Name, fields = #e_vars{type_map = FieldTypeMap}}, StructMap, UsedStructs) ->
	check_struct_field(maps:to_list(FieldTypeMap), StructMap, [Name | UsedStructs]).

-spec check_struct_field([{atom(), e_type()}], #{atom() => #e_struct{}}, [atom()]) -> ok.
check_struct_field([{_, FieldType} | RestFields], StructMap, UsedStructs) ->
	case contain_struct(FieldType, StructMap) of
		{yes, StructName} ->
			check_struct_field_sub(StructName, StructMap, UsedStructs);
		no ->
			check_struct_field(RestFields, StructMap, UsedStructs)
	end;
check_struct_field([], _, _) ->
	ok.

check_struct_field_sub(StructName, StructMap, UsedStructs) ->
	case e_util:value_in_list(StructName, UsedStructs) of
		true ->
			throw({recur, lists:reverse([StructName | UsedStructs])});
		false ->
			check_struct_object(maps:get(StructName, StructMap), StructMap, UsedStructs)
	end.

-spec contain_struct(e_type(), #{atom() => #e_struct{}}) -> {yes, atom()} | no.
contain_struct(#e_basic_type{class = struct, p_depth = 0, tag = Name, loc = Loc}, StructMap) ->
	case maps:find(Name, StructMap) of
		{ok, _} ->
			{yes, Name};
		_ ->
			e_util:ethrow(Loc, "undefined struct \"~s\"", [Name])
	end;
contain_struct(#e_array_type{elem_type = BaseType}, StructMap) ->
	contain_struct(BaseType, StructMap);
contain_struct(_, _) ->
	no.


-type interface_context() :: {#{atom() => #e_fn_type{}}, #{atom() => #e_struct{}}}.

-spec eliminate_dot_in_ast(e_ast(), #e_vars{}, interface_context()) -> e_ast().
eliminate_dot_in_ast([#e_function{stmts = Stmts0} = Fn | Rest], GlobalVars, {FnTypeMap, StructMap} = Ctx) ->
	Vars = e_util:merge_vars(GlobalVars, Fn#e_function.vars, ignore_tag),
	Stmts1 = eliminate_dot(Stmts0, {Vars, FnTypeMap, StructMap, #e_basic_type{}}),
	[Fn#e_function{stmts = Stmts1} | eliminate_dot_in_ast(Rest, GlobalVars, Ctx)];
eliminate_dot_in_ast([Any | Rest], GlobalVars, Ctx) ->
	[Any | eliminate_dot_in_ast(Rest, GlobalVars, Ctx)];
eliminate_dot_in_ast([], _, _) ->
	[].

-spec eliminate_dot_in_stmts([e_stmt()], #e_vars{}, interface_context()) -> [e_stmt()].
eliminate_dot_in_stmts(Stmts, Vars, {FnTypeMap, StructMap}) ->
	eliminate_dot(Stmts, {Vars, FnTypeMap, StructMap, #e_basic_type{}}).

%% This `context()` is the same as the one in `e_type.erl`.
-type context() ::
	{
	GlobalVarTypes :: #e_vars{},
	FnTypeMap :: #{atom() := #e_fn_type{}},
	StructMap :: #{atom() => #e_struct{}},
	ReturnType :: e_type()
	}.

-spec eliminate_dot([e_stmt()], context()) -> [e_stmt()].
eliminate_dot(Stmts0, Ctx) ->
	Stmts1 = e_util:expr_map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Stmts0),
	e_util:eliminate_pointer(Stmts1).

%% `a.b` will be converted to `(a@ + OFFSET_OF_b)^`.
-spec eliminate_dot_in_expr(e_expr(), context()) -> e_expr().
eliminate_dot_in_expr(#e_op{tag = '.', loc = Loc} = Op, {_, _, StructMap, _} = Ctx) ->
	#e_op{data = [O, #e_varref{name = FieldName}]} = Op,
	#e_basic_type{class = struct, tag = Name, p_depth = 0} = e_type:type_of_node(O, Ctx),
	{ok, #e_struct{fields = #e_vars{offset_map = FieldOffsetMap}}} = maps:find(Name, StructMap),
	{ok, {Offset, Size}} = maps:find(FieldName, FieldOffsetMap),
	A = #e_op{tag = '@', data = [eliminate_dot_in_expr(O, Ctx)], loc = Loc},
	B = #e_op{tag = '+', data = [A, #e_integer{value = Offset, loc = Loc}], loc = Loc},
	#e_op{tag = '^', data = [B, #e_integer{value = Size, loc = Loc}], loc = Loc};
eliminate_dot_in_expr(#e_op{tag = {call, Callee}, data = Args} = Op, Ctx) ->
	Op#e_op{tag = {call, eliminate_dot_in_expr(Callee, Ctx)}, data = lists:map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Args)};
eliminate_dot_in_expr(#e_op{data = Args} = Op, Ctx) ->
	Op#e_op{data = lists:map(fun(E) -> eliminate_dot_in_expr(E, Ctx) end, Args)};
eliminate_dot_in_expr(#e_type_convert{expr = Expr} = C, Ctx) ->
	C#e_type_convert{expr = eliminate_dot_in_expr(Expr, Ctx)};
eliminate_dot_in_expr(Any, _) ->
	Any.

