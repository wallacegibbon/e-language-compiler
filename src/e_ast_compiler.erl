-module(e_ast_compiler).
-export([compile_from_raw_ast/2]).
-include("e_record_definition.hrl").

-type e_compile_options() :: map().

-spec compile_from_raw_ast(e_ast(), e_compile_options()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_from_raw_ast(AST, CustomCompileOptions) ->
	CompileOptions = maps:merge(default_compiler_options(), CustomCompileOptions),
	{GlobalVars00, AST00, InitCode00} = e_var:fetch_vars(AST),

	{FnTypeMap00, StructMap00} = e_util:make_function_and_struct_map_from_ast(AST00),
	%% Once the AST got constructed, we check the recursive definition problem of struct.
	e_struct:check_struct_recursion(StructMap00),

	%% type checking
	e_type:check_types_in_ast(AST00, GlobalVars00, {FnTypeMap00, StructMap00}),
	e_type:check_type_in_stmts(InitCode00, GlobalVars00, {FnTypeMap00, StructMap00}),

	#{pointer_width := PointerWidth} = CompileOptions,
	%% Fill offset of variables and struct fields.
	AST10 = e_size:fill_offsets_in_stmts(AST00, {StructMap00, PointerWidth}),
	%% Struct info is updated, so StructMap needs to be updated, too.
	{_, StructMap10} = e_util:make_function_and_struct_map_from_ast(AST10),
	GlobalVars10 = e_size:fill_offsets_in_vars(GlobalVars00, {StructMap10, PointerWidth}),

	%% Expand expressions like `sizeof` and `alignof`.
	AST20 = e_size:expand_kw_in_ast(AST10, {StructMap10, PointerWidth}),
	%% Initializing code for global variables are not in main ast, do not forget them.
	InitCode10 = e_size:expand_kw_in_stmts(InitCode00, {StructMap10, PointerWidth}),
	%% sizeof expressions are expanded, so StructMap needs to be updated
	{_, StructMap20} = e_util:make_function_and_struct_map_from_ast(AST20),

	%% expand init exprs like A{a = 1} and {1, 2, 3}
	AST30 = e_init_expr:expand_in_ast(AST20, StructMap20),
	InitCode20 = e_init_expr:expand_in_stmts(InitCode10, StructMap20),

	%% convert `.` into `@`, `+` and `^`
	AST40 = e_struct:eliminate_dot_in_ast(AST30, GlobalVars10, {FnTypeMap00, StructMap20}),
	InitCode30 = e_struct:eliminate_dot_in_stmts(InitCode20, GlobalVars10, {FnTypeMap00, StructMap20}),

	AST50 = e_varref:varref_to_offset_in_ast(AST40, {GlobalVars10, FnTypeMap00}),
	InitCode40 = e_varref:varref_to_offset_in_stmts(InitCode30, {GlobalVars10, FnTypeMap00}),

	{AST50, GlobalVars10, InitCode40}.

-spec default_compiler_options() -> e_compile_options().
default_compiler_options() ->
	#{pointer_width => 8}.

