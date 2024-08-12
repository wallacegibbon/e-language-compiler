%% There are 3 kinds of data types in E language: `basic type`, `array type` and `function type`.
-record(e_basic_type,
	{
	line = 0 :: integer(),
	p_depth = 0 :: integer(),
	class = void :: struct | integer | float | void | any,
	%% tag can be primitive tag like `u8`, `f32` for `integer` and `float`, or struct name for `struct`
	%% full list of primitive tags: u8|i8|u16|i16|u32|i32|u64|i64|byte|uptr|iptr|usize|isize|f64|f32|void|any.
	tag :: atom()
	}).

-record(e_array_type,
	{
	line = 0 :: integer(),
	elem_type :: e_type(),
	length :: integer()
	}).

-record(e_fn_type,
	{
	line = 0 :: integer(),
	params = [] :: [e_type()],
	ret :: e_type()
	}).

-type e_type() :: #e_basic_type{} | #e_array_type{} | #e_fn_type{}.

-record(e_goto_label,
	{
	line = 0 :: integer(),
	name :: atom()
	}).

%% Variable definition. Will be used on both variable definition and struct field definition.
-record(e_vardef,
	{
	line = 0 :: integer(),
	name :: atom(),
	type :: e_type(),
	init_value = none :: e_expr() | none
	}).

-record(e_varref,
	{
	line = 0 :: integer(),
	name :: atom()
	}).

%% `e_vars` is used by functions and structs to hold variables (including parameters) and fields.
-record(e_vars,
	{
	%% `names` keeps the order that variables/fields got declared.
	names = [] :: [#e_varref{}],
	type_map = #{} :: #{atom() => e_type()},
	offset_map = #{} :: #{atom() => non_neg_integer()},
	%% The align of the biggest element in this `e_vars`.
	align = 0 :: non_neg_integer(),
	%% The whole size of this `e_vars`.
	size = 0 :: non_neg_integer()
	}).

-record(e_function,
	{
	line = 0 :: integer(),
	name :: atom(),
	%% `vars` contains both parameters and variables.
	vars = #e_vars{} :: #e_vars{},
	param_names = [] :: [atom()],
	type :: #e_fn_type{},
	%% TODO: remove `labels` here. (labels should be in `stmts`).
	labels = [] :: [#e_goto_label{}],
	stmts = [] :: [e_stmt()]
	}).

-record(e_struct,
	{
	line = 0 :: integer(),
	name :: atom(),
	fields = #e_vars{} :: #e_vars{},
	default_value_map = #{} :: #{atom() => e_expr()}
	}).

-record(e_struct_init_raw_expr,
	{
	line = 0 :: integer(),
	name :: atom(),
	fields = [] :: [e_expr()]
	}).

-record(e_struct_init_expr,
	{
	line = 0 :: integer(),
	name :: atom(),
	field_value_map = #{} :: #{atom() => e_expr()}
	}).

-record(e_array_init_expr,
	{
	line = 0 :: integer(),
	elements = [] :: [e_expr()]
	}).

-record(e_type_convert,
	{
	line = 0 :: integer(),
	expr :: e_expr(),
	type :: e_type()
	}).

-record(e_op,
	{
	line = 0 :: integer(),
	tag :: e_op_tag(),
	data = [] :: [e_expr()]
	}).

-record(e_integer,
	{
	line = 0 :: integer(),
	value :: integer()
	}).

-record(e_float,
	{
	line = 0 :: integer(),
	value :: float()
	}).

-record(e_string,
	{
	line = 0 :: integer(),
	value :: string()
	}).

-record(e_goto_stmt,
	{
	line = 0 :: integer(),
	expr :: e_expr()
	}).

-record(e_return_stmt,
	{
	line = 0 :: integer(),
	expr :: e_expr()
	}).

-record(e_while_stmt,
	{
	line = 0 :: integer(),
	condi :: e_expr(),
	stmts = [] :: [e_stmt()]
	}).

-record(e_if_stmt,
	{
	line = 0 :: integer(),
	condi :: e_expr(),
	then = [] :: [e_stmt()],
	else = [] :: [e_stmt()]
	}).

-record(e_struct_raw,
	{
	line = 0 :: integer(),
	name :: atom(),
	fields = [] :: [e_expr()]
	}).

-record(e_function_raw,
	{
	line = 0 :: integer(),
	name :: atom(),
	params = [] :: [e_stmt()],
	ret_type :: e_type(),
	stmts = [] :: [e_stmt()]
	}).

-type e_op_tag() :: '*' | '/' | '+' | '-' | '@' | '^' | 'rem' | 'and' | 'or' | 'band' | 'bor' | 'bxor' | 'bsl' | 'bsr' | '~' | '!' | '.' | '=' | '>' | '<' | '>=' | '<=' | '!=' | '==' | {call, e_expr()} | {sizeof, e_type()}.

-type e_expr() :: #e_op{} | #e_integer{} | #e_float{} | #e_string{} | #e_varref{} | #e_struct_init_expr{} | #e_array_init_expr{}.
-type e_stmt() :: #e_if_stmt{} | #e_while_stmt{} | #e_goto_stmt{} | #e_goto_label{} | #e_return_stmt{} | e_expr().

-type e_ast_raw() :: [#e_function_raw{} | #e_struct_raw{} | #e_vardef{}].
-type e_ast_elem() :: #e_function{} | #e_struct{} | #e_vardef{}.
-type e_ast() :: [e_ast_elem()].

