-record(fn_type, {
	line = 0 :: integer(),
	params = [] :: [e_type()],
	ret :: e_type()
}).

-record(function, {
	line = 0 :: integer(),
	name :: atom(),
	type :: #fn_type{},
	param_names = [] :: [atom()],
	var_type_map :: var_type_map(),
	labels = [] :: [e_expr()],
	stmts = [] :: [e_ast()]
}).

-record(struct, {
	line = 0 :: integer(),
	name :: atom(),
	size = -1 :: integer(),
	field_type_map :: var_type_map(),
	field_offset_map = #{} :: #{atom() := integer()},
	field_names = [] :: [any()],
	field_default_value_map = #{} :: #{atom() := e_expr()}
}).

-record(basic_type, {
	line = 0 :: integer(),
	p_depth = 0 :: integer(),
	class :: struct | integer | float | void | any,
	tag :: atom()
}).

-record(array_type, {
	line = 0 :: integer(),
	elem_type :: e_type(),
	length :: integer()
}).

-record(struct_init_raw_expr, {
	line = 0 :: integer(),
	name :: atom(),
	fields = [] :: [e_expr()]
}).

-record(struct_init_expr, {
	line = 0 :: integer(),
	name :: atom(),
	field_names = [] :: [any()],
	field_value_map = #{} :: #{atom() := e_expr()}
}).

-record(array_init_expr, {
	line = 0 :: integer(),
	elements = [] :: [e_expr()]
}).

-record(function_raw, {
	line = 0 :: integer(),
	name :: atom(),
	params = [] :: [e_expr()],
	ret_type :: e_type(),
	stmts = [] :: [e_expr()]
}).

-record(struct_raw, {
	line = 0 :: integer(),
	name :: atom(),
	fields = [] :: [e_expr()]
}).

-record(return_stmt, {
	line = 0 :: integer(),
	expr :: e_expr()
}).

-record(while_stmt, {
	line = 0 :: integer(),
	condi :: e_expr(),
	stmts = [] :: [e_expr()]
}).

-record(if_stmt, {
	line = 0 :: integer(),
	condi :: e_expr(),
	then = [] :: [e_expr()],
	else = [] :: [e_expr()]
}).

-record(var_def, {
	line = 0 :: integer(),
	name :: atom(),
	type :: e_type(),
	init_value = none :: any()
}).

-record(var_ref, {
	line = 0 :: integer(),
	name :: atom()
}).

-record(integer, {
	line = 0 :: integer(),
	value :: integer()
}).

-record(float, {
	line = 0 :: integer(),
	value :: float()
}).

-record(string, {
	line = 0 :: integer(),
	value :: string()
}).

-type expr_tag() ::
	'*' | '/' | 'div' | '+' | '-' | 'rem' | 'sizeof' |
	{call, atom()} |
	any().

-record(e_expr, {
	line = 0 :: integer(),
	tag :: expr_tag(),
	data :: [e_expr()]
}).

-type e_expr() :: #e_expr{}.

-record(goto_label, {
	line = 0 :: integer(),
	name :: atom()
}).

-record(goto_stmt, {
	line = 0 :: integer(),
	expr :: e_expr()
}).

-record(type_convert, {
	line = 0 :: integer(),
	expr :: e_expr(),
	type :: e_type()
}).

%% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type e_type() :: #basic_type{} | #array_type{} | #fn_type{} | any().
-type e_stmt() :: #if_stmt{} | #while_stmt{} | #goto_stmt{} | #return_stmt{}.
-type struct_type_map() :: #{atom() := #struct{}}.
-type fn_type_map() :: #{atom() := #fn_type{}}.
-type fn_ret_type_map() :: #{atom() := e_type()}.
-type var_type_map() :: #{atom() := e_type()}.
-type e_ast() :: [e_expr()].

