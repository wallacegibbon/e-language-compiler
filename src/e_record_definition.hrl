-record(var_def, {
	line = 0 :: integer(),
	name :: atom(),
	type :: e_type(),
	init_value = none :: e_expr()
}).

-record(var_ref, {
	line = 0 :: integer(),
	name :: atom()
}).

-record(struct, {
	line = 0 :: integer(),
	name :: atom(),
	size = -1 :: integer(),
	field_type_map :: var_type_map(),
	field_offset_map = #{} :: #{atom() := integer()},
	field_names = [] :: [#var_ref{}],
	field_default_value_map = #{} :: #{atom() := e_expr()}
}).

-record(struct_raw, {
	line = 0 :: integer(),
	name :: atom(),
	fields = [] :: [e_expr()]
}).

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
	labels = [] :: [#goto_label{}],
	stmts = [] :: [e_stmt()]
}).

-record(function_raw, {
	line = 0 :: integer(),
	name :: atom(),
	params = [] :: [e_expr()],
	ret_type :: e_type(),
	stmts = [] :: [e_stmt()]
}).

-record(basic_type, {
	line = 0 :: integer(),
	p_depth = 0 :: integer(),
	class = void :: struct | integer | float | void | any,
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
	field_names = [] :: [#var_ref{}],
	field_value_map = #{} :: #{atom() := e_expr()}
}).

-record(array_init_expr, {
	line = 0 :: integer(),
	elements = [] :: [e_expr()]
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
	'*' | '/' | '+' | '-' | '@' | '^' | 'rem' | 'and' | 'or' | 'band' | 'bor' | 'bxor' | 'bsl' | 'bsr' | '~' | '!' | '.' |
	'=' | '>' | '<' | '>=' | '<=' | '!=' | '==' | {call, e_expr()} | {sizeof, e_type()}.

-record(e_expr, {
	line = 0 :: integer(),
	tag :: expr_tag(),
	data = [] :: [e_expr()]
}).

-type e_expr() :: #e_expr{} | #integer{} | #float{} | #string{} | #var_ref{} | #struct_init_expr{} | #array_init_expr{}.

%% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type e_type() :: #basic_type{} | #array_type{} | #fn_type{}.

-type expr_stmt() :: e_expr().

-type e_stmt() :: #if_stmt{} | #while_stmt{} | #goto_stmt{} | #goto_label{} | #return_stmt{} | expr_stmt().

-type struct_type_map() :: #{atom() := #struct{}}.

-type fn_type_map() :: #{atom() := #fn_type{}}.

-type var_type_map() :: #{atom() := e_type()}.

-type e_ast_raw_elem() :: #function_raw{} | #struct_raw{} | #var_def{}.
-type e_ast_raw() :: [e_ast_raw_elem()].

-type e_ast_elem() :: #function{} | #struct{} | #var_def{}.
-type e_ast() :: [e_ast_elem()].

