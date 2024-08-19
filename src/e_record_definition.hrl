-type location() :: {non_neg_integer(), non_neg_integer()}.

%% There are 3 kinds of data types in E language: `basic type`, `array type` and `function type`.
-record(e_basic_type,
	{
	loc = {0, 0} :: location(),
	p_depth = 0 :: integer(),
	class = void :: struct | integer | float | void | any,
	%% tag can be primitive tag like `u8`, `f32` for `integer` and `float`, or struct name for `struct`
	%% full list of primitive tags: u8|i8|u16|i16|u32|i32|u64|i64|byte|uptr|iptr|usize|isize|f64|f32|void|any.
	tag :: atom()
	}).

-record(e_array_type,
	{
	loc = {0, 0} :: location(),
	elem_type :: e_type(),
	length :: integer()
	}).

-record(e_fn_type,
	{
	loc = {0, 0} :: location(),
	params = [] :: [e_type()],
	ret :: e_type()
	}).

-record(e_typeof,
	{
	loc = {0, 0} :: location(),
	expr :: e_expr()
	}).

-type e_type() :: #e_basic_type{} | #e_array_type{} | #e_fn_type{} | #e_typeof{}.

-record(e_label,
	{
	loc = {0, 0} :: location(),
	name :: atom()
	}).

%% Variable definition. Will be used on both variable definition and struct field definition.
-record(e_vardef,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	type :: e_type(),
	init_value = none :: e_expr() | none
	}).

-record(e_varref,
	{
	loc = {0, 0} :: location(),
	name :: atom()
	}).

-type e_var_type() :: none | local | global.

-type var_offset() :: {Offset :: non_neg_integer(), Size :: pos_integer()}.

%% `e_vars` is used by functions and structs to hold variables (including parameters) and fields.
-record(e_vars,
	{
	%% `names` keeps the order that variables/fields got declared.
	names = [] :: [#e_varref{}],
	type_map = #{} :: #{atom() => e_type()},
	offset_map = #{} :: #{atom() => var_offset()},
	%% The alignment of e_vars is the alignment of the biggest element in this `e_vars`.
	%% The alignment should be higher than or equals `1`. `0` is the uninitialized value for alignments.
	align = 0 :: non_neg_integer(),
	%% The whole size of this `e_vars`.
	size = 0 :: non_neg_integer(),
	tag = none :: e_var_type()
	}).

-record(e_function,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	%% `vars` contains both parameters and variables.
	vars = #e_vars{} :: #e_vars{},
	param_names = [] :: [atom()],
	type :: #e_fn_type{},
	stmts = [] :: [e_stmt()]
	}).

-record(e_struct,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	fields = #e_vars{} :: #e_vars{},
	default_value_map = #{} :: #{atom() => e_expr()}
	}).

-record(e_struct_init_raw_expr,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	fields = [] :: [e_expr()]
	}).

-record(e_struct_init_expr,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	field_value_map = #{} :: #{atom() => e_expr()}
	}).

-record(e_array_init_expr,
	{
	loc = {0, 0} :: location(),
	elements = [] :: [e_expr()]
	}).

-record(e_type_convert,
	{
	loc = {0, 0} :: location(),
	expr :: e_expr(),
	type :: e_type()
	}).

-record(e_op,
	{
	loc = {0, 0} :: location(),
	tag :: e_op_tag(),
	data = [] :: [e_expr()]
	}).

-record(e_integer,
	{
	loc = {0, 0} :: location(),
	value :: integer()
	}).

-record(e_float,
	{
	loc = {0, 0} :: location(),
	value :: float()
	}).

-record(e_string,
	{
	loc = {0, 0} :: location(),
	value :: string()
	}).

-record(e_goto_stmt,
	{
	loc = {0, 0} :: location(),
	label :: atom()
	}).

-record(e_return_stmt,
	{
	loc = {0, 0} :: location(),
	expr :: e_expr()
	}).

-record(e_while_stmt,
	{
	loc = {0, 0} :: location(),
	condi :: e_expr(),
	stmts = [] :: [e_stmt()]
	}).

-record(e_if_stmt,
	{
	loc = {0, 0} :: location(),
	condi :: e_expr(),
	then = [] :: [e_stmt()],
	'else' = [] :: [e_stmt()]
	}).

-record(e_struct_raw,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	fields = [] :: [e_stmt()]
	}).

-record(e_function_raw,
	{
	loc = {0, 0} :: location(),
	name :: atom(),
	params = [] :: [e_stmt()],
	ret_type :: e_type(),
	stmts = [] :: [e_stmt()]
	}).

-type e_op_tag() :: '*' | '/' | '+' | '-' | '@' | '^' | 'rem' | 'and' | 'or' | 'band' | 'bor' | 'bxor' | 'bsl' | 'bsr' | '~' | '!' | '.' | '=' | '>' | '<' | '>=' | '<=' | '!=' | '==' | {call, e_expr()} | {sizeof, e_type()} | {alignof, e_type()}.

-type e_expr() :: #e_op{} | #e_integer{} | #e_float{} | #e_string{} | #e_varref{} | #e_struct_init_expr{} | #e_array_init_expr{}.
-type e_stmt() :: #e_if_stmt{} | #e_while_stmt{} | #e_return_stmt{} | #e_goto_stmt{} | #e_label{} | e_expr().

-type e_ast_raw_elem() :: #e_function_raw{} | #e_struct_raw{} | #e_vardef{} | e_stmt().
-type e_ast_raw() :: [e_ast_raw_elem()].
-type e_ast_elem() :: #e_function{} | #e_struct{} | #e_vardef{} | e_stmt().
-type e_ast() :: [e_ast_elem()].

