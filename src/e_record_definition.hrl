-record(function_type,
        {line = 0 :: integer(),
         parameters = [] :: [e_type()],
         ret :: e_type()}).

-record(function,
        {line = 0 :: integer(),
         name :: atom(),
         type :: #function_type{},
         param_names = [] :: [atom()],
         var_type_map :: var_type_map(),
         labels = [] :: [e_expr()],
         statements = [] :: [e_ast()]}).

-record(struct,
        {line = 0 :: integer(),
         name :: atom(),
         size = -1 :: integer(),
         field_type_map :: var_type_map(),
         field_offset_map = #{} :: #{atom() := integer()},
         field_names = [] :: [any()],
         field_default_value_map = #{} :: #{atom() := e_expr()}}).

-record(basic_type,
        {line = 0 :: integer(),
         pdepth = 0 :: integer(),
         class :: struct | integer | float | void | any,
         tag :: atom()}).

-record(array_type,
        {line = 0 :: integer(),
         elemtype :: e_type(),
         length :: integer()}).

-record(struct_init_expr_raw,
        {line = 0 :: integer(),
         name :: atom(),
         fields = [] :: [e_expr()]}).

-record(struct_init_expr,
        {line = 0 :: integer(),
         name :: atom(),
         field_names = [] :: [any()],
         field_value_map = #{} :: #{atom() := e_expr()}}).

-record(array_init_expr,
        {line = 0 :: integer(),
         elements = [] :: [e_expr()]}).

-record(function_raw,
        {line = 0 :: integer(),
         name :: atom(),
         parameters = [] :: [e_expr()],
         ret_type :: e_type(),
         statements = [] :: [e_expr()]}).

-record(struct_raw,
        {line = 0 :: integer(),
         name :: atom(),
         fields = [] :: [e_expr()]}).

-record(return_statement,
        {line = 0 :: integer(),
         expression :: e_expr()}).

-record(while_statement,
        {line = 0 :: integer(),
         condition :: e_expr(),
         statements = [] :: [e_expr()]}).

-record(if_statement,
        {line = 0 :: integer(),
         condition :: e_expr(),
         then = [] :: [e_expr()],
         else = [] :: [e_expr()]}).

-record(variable_definition,
        {line = 0 :: integer(),
         name :: atom(),
         type :: e_type(),
         init_value = none :: any()}).

-record(variable_reference,
        {line = 0 :: integer(),
         name :: atom()}).

-record(integer,
        {line = 0 :: integer(),
         value :: integer()}).

-record(float,
        {line = 0 :: integer(),
         value :: float()}).

-record(string,
        {line = 0 :: integer(),
         value :: string()}).

-record(operator_expression1,
        {line = 0 :: integer(),
         operator :: atom(),
         operand :: e_expr()}).

-record(operator_expression2,
        {line = 0 :: integer(),
         operator :: atom(),
         operand1 :: e_expr(),
         operand2 :: e_expr()}).

-record(call_expr,
        {line = 0 :: integer(),
         fn :: e_expr(),
         args = [] :: [e_expr()]}).

-record(sizeof_expression,
        {line = 0 :: integer(),
         type :: e_type()}).

-record(goto_label,
        {line = 0 :: integer(),
         name :: atom()}).

-record(goto_statement,
        {line = 0 :: integer(),
         expression :: e_expr()}).

-record(type_convert,
        {line = 0 :: integer(),
         expression :: e_expr(),
         type :: e_type()}).

% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type e_type() :: #basic_type{} | #array_type{} | #function_type{} | any().

-type e_statement() :: #if_statement{} | #while_statement{} | #goto_statement{} | #return_statement{}.
-type e_expr() :: #operator_expression1{} | #operator_expression2{} | #call_expr{} | #sizeof_expression{} | any().

-type struct_type_map() :: #{atom() := #struct{}}.

-type fn_type_map() :: #{atom() := #function_type{}}.

-type fn_ret_type_map() :: #{atom() := e_type()}.

-type var_type_map() :: #{atom() := e_type()}.

-type e_ast() :: [e_expr()].
