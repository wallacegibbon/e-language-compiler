-module(e_ast_compiler).
-export([compile_from_raw_ast/2]).
-include("e_record_definition.hrl").

-type e_compile_options() :: map().

-spec compile_from_raw_ast(e_ast(), e_compile_options()) -> {e_ast(), e_var_type_map(), e_ast()}.
compile_from_raw_ast(AST, CustomCompileOptions) ->
	CompileOptions = maps:merge(default_compiler_options(), CustomCompileOptions),
	{AST2, VarTypeMap, InitCode0} = e_variable:fetch_variables(AST),
	{FnTypeMap, StructMap0} = e_util:make_function_and_struct_map_from_ast(AST2),

	%% struct recursion is not allowed.
	ensure_no_recursive_struct(StructMap0),
	#{pointer_width := PointerWidth} = CompileOptions,
	%% calculate struct size, filed offsets
	AST3 = e_size:fill_struct_info(AST2, {StructMap0, PointerWidth}),

	%% struct size is updated, so StructMap needs to be updated, too
	{_, StructMap1} = e_util:make_function_and_struct_map_from_ast(AST3),
	%% expand sizeof expression
	Ctx1 = {StructMap1, PointerWidth},
	AST4 = e_size:expand_sizeof(AST3, Ctx1),

	%% Initializing code for global variables are not in main ast,
	%% do not forget it
	InitCode1 = e_size:expand_sizeof_in_exprs(InitCode0, Ctx1),
	%% sizeof expressions are expanded, so StructMap needs to be updated
	{_, StructMap2} = e_util:make_function_and_struct_map_from_ast(AST4),

	%% type checking & converting
	Maps = {FnTypeMap, StructMap2},

	e_type:check_types_in_ast(AST4, VarTypeMap, Maps),
	e_type:check_type_in_stmts(InitCode1, VarTypeMap, Maps),

	%% expand init exprs like A{a=1} and {1,2,3}
	AST5 = e_init_expr:expand_in_function(AST4, StructMap2),
	InitCode2 = e_init_expr:expand_init_expr(InitCode1, StructMap2),

	%% convert `.` into `@`, `+` and `^`
	AST6 = e_struct:eliminate_dot_in_ast(AST5, VarTypeMap, Maps),
	InitCode3 = e_struct:eliminate_dot_in_stmts(InitCode2, VarTypeMap, Maps),

	{AST6, VarTypeMap, InitCode3}.

-spec default_compiler_options() -> e_compile_options().
default_compiler_options() ->
	#{pointer_width => 8}.

-spec ensure_no_recursive_struct(e_struct_type_map()) -> ok.
ensure_no_recursive_struct(StructTypeMap) ->
	maps:foreach(fun(_, S) -> check_struct_recursive(S, StructTypeMap) end, StructTypeMap).

-spec check_struct_recursive(#e_struct{}, e_struct_type_map()) -> ok.
check_struct_recursive(#e_struct{name = Name, line = Line} = Struct, StructTypeMap) ->
	try
		check_struct_object(Struct, StructTypeMap, [])
	catch
		{recur, Chain} ->
			e_util:ethrow(Line, "recursive struct ~s -> ~w", [Name, Chain])
	end.

-spec check_struct_object(#e_struct{}, e_struct_type_map(), [atom()]) -> ok | {recur, [any()]}.
check_struct_object(#e_struct{name = Name, field_type_map = FieldTypes}, StructMap, UsedStructs) ->
	check_struct_field(maps:to_list(FieldTypes), StructMap, [Name | UsedStructs]).

-spec check_struct_field([{atom(), e_type()}], e_struct_type_map(), [atom()]) -> ok.
check_struct_field([{_, FieldType} | RestFields], StructMap, UsedStructs) ->
	case contain_struct(FieldType) of
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

-spec contain_struct(e_type()) -> {yes, atom()} | no.
contain_struct(#e_basic_type{class = struct, p_depth = 0, tag = Name}) ->
	{yes, Name};
contain_struct(#e_array_type{elem_type = BaseType}) ->
	contain_struct(BaseType);
contain_struct(_) ->
	no.

