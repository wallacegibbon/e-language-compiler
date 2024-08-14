-module(e_ast_compiler).
-export([compile_from_raw_ast/2]).
-include("e_record_definition.hrl").

-type e_compile_options() :: map().

-spec compile_from_raw_ast(e_ast(), e_compile_options()) -> {e_ast(), #e_vars{}, e_ast()}.
compile_from_raw_ast(AST, CustomCompileOptions) ->
	CompileOptions = maps:merge(default_compiler_options(), CustomCompileOptions),
	{GlobalVars0, AST00, InitCode0} = e_variable:fetch_variables(AST),

	{FnTypeMap, StructMap0} = e_util:make_function_and_struct_map_from_ast(AST00),

	%% Once the StructMap is constructed, we check the recursion problem.
	e_struct:check_struct_recursion(StructMap0),

	#{pointer_width := PointerWidth} = CompileOptions,

	%% Fill offset of variables and struct fields.
	AST10 = e_size:fill_offsets_in_stmts(AST00, {StructMap0, PointerWidth}),
	%% Struct info is updated, so StructMap needs to be updated, too.
	{_, StructMap1} = e_util:make_function_and_struct_map_from_ast(AST10),
	GlobalVars = e_size:fill_offsets_in_vars(GlobalVars0, {StructMap1, PointerWidth}),

	%% Expand expressions like `sizeof` and `alignof`.
	AST20 = e_size:expand_kw_in_ast(AST10, {StructMap1, PointerWidth}),
	%% Initializing code for global variables are not in main ast, do not forget them.
	InitCode1 = e_size:expand_kw_in_stmts(InitCode0, {StructMap1, PointerWidth}),
	%% sizeof expressions are expanded, so StructMap needs to be updated
	{_, StructMap2} = e_util:make_function_and_struct_map_from_ast(AST20),

	%% type checking & converting
	MapCtx = {FnTypeMap, StructMap2},

	e_type:check_types_in_ast(AST20, GlobalVars, MapCtx),
	e_type:check_type_in_stmts(InitCode1, GlobalVars, MapCtx),

	%% expand init exprs like A{a = 1} and {1, 2, 3}
	AST30 = e_init_expr:expand_in_ast(AST20, StructMap2),
	InitCode2 = e_init_expr:expand_in_stmts(InitCode1, StructMap2),

	%% convert `.` into `@`, `+` and `^`
	AST40 = e_struct:eliminate_dot_in_ast(AST30, GlobalVars, MapCtx),
	InitCode3 = e_struct:eliminate_dot_in_stmts(InitCode2, GlobalVars, MapCtx),

	AST50 = e_ref_trans:varref_to_offset_in_ast(AST40, {GlobalVars, FnTypeMap}),
	InitCode4 = e_ref_trans:varref_to_offset_in_stmts(InitCode3, {GlobalVars, FnTypeMap}),

	{AST50, GlobalVars, InitCode4}.

-spec default_compiler_options() -> e_compile_options().
default_compiler_options() ->
	#{pointer_width => 8}.

