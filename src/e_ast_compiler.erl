-module(e_ast_compiler).
-export([compile_from_raw_ast/2]).
-include("e_record_definition.hrl").

-type e_compile_options() :: map().

-spec compile_from_raw_ast(e_ast(), e_compile_options()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_from_raw_ast(AST, CustomCompileOptions) ->
	CompileOptions = maps:merge(default_compiler_options(), CustomCompileOptions),
	{GlobalVars0, AST02, InitCode0} = e_variable:fetch_variables(AST),
	{FnTypeMap, StructMap0} = e_util:make_function_and_struct_map_from_ast(AST02),

	%% Struct recursion is not allowed.
	ensure_no_recursive_struct(StructMap0),
	#{pointer_width := PointerWidth} = CompileOptions,

	%% Calculate struct size, filed offsets
	SizeContext = {StructMap0, PointerWidth},
	%% Fill offset of variables and struct fields.
	AST04 = e_size:fill_offsets(AST02, SizeContext),
	GlobalVars = e_size:fill_var_offsets(GlobalVars0, SizeContext),

	%% Struct size is updated, so StructMap needs to be updated, too
	{_, StructMap1} = e_util:make_function_and_struct_map_from_ast(AST04),
	%% Expand `sizeof` expression
	Ctx1 = {StructMap1, PointerWidth},
	AST05 = e_size:expand_sizeof_in_ast(AST04, Ctx1),

	%% Initializing code for global variables are not in main ast,
	%% do not forget it
	InitCode1 = e_size:expand_sizeof_in_stmts(InitCode0, Ctx1),
	%% sizeof expressions are expanded, so StructMap needs to be updated
	{_, StructMap2} = e_util:make_function_and_struct_map_from_ast(AST05),

	%% type checking & converting
	Maps = {FnTypeMap, StructMap2},

	e_type:check_types_in_ast(AST05, GlobalVars, Maps),
	e_type:check_type_in_stmts(InitCode1, GlobalVars, Maps),

	%% expand init exprs like A{a = 1} and {1, 2, 3}
	AST06 = e_init_expr:expand_in_function(AST05, StructMap2),
	InitCode2 = e_init_expr:expand_init_expr(InitCode1, StructMap2),

	%% convert `.` into `@`, `+` and `^`
	AST07 = e_struct:eliminate_dot_in_ast(AST06, GlobalVars, Maps),
	InitCode3 = e_struct:eliminate_dot_in_stmts(InitCode2, GlobalVars, Maps),

	{AST07, GlobalVars, InitCode3}.

-spec default_compiler_options() -> e_compile_options().
default_compiler_options() ->
	#{pointer_width => 8}.

-spec ensure_no_recursive_struct(#{atom() => #e_struct{}}) -> ok.
ensure_no_recursive_struct(StructTypeMap) ->
	maps:foreach(fun(_, S) -> check_struct_recursive(S, StructTypeMap) end, StructTypeMap).

-spec check_struct_recursive(#e_struct{}, #{atom() => #e_struct{}}) -> ok.
check_struct_recursive(#e_struct{name = Name, line = Line} = Struct, StructTypeMap) ->
	try
		check_struct_object(Struct, StructTypeMap, [])
	catch
		{recur, Chain} ->
			e_util:ethrow(Line, "recursive struct ~s -> ~w", [Name, Chain])
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
contain_struct(#e_basic_type{class = struct, p_depth = 0, tag = Name, line = Line}, StructMap) ->
	case maps:find(Name, StructMap) of
		{ok, _} ->
			{yes, Name};
		_ ->
			e_util:ethrow(Line, "undefined struct \"~s\"", [Name])
	end;
contain_struct(#e_array_type{elem_type = BaseType}, StructMap) ->
	contain_struct(BaseType, StructMap);
contain_struct(_, _) ->
	no.

