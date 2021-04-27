-record(function,
        {line :: integer(),
         name :: atom(),
         type :: any(),
         param_names :: [atom()],
         var_types :: #{atom() := e_type()},
         labels :: [e_expr()],
         exprs :: [e_expr()]}).

-record(struct,
        {line :: integer(),
         name :: atom(),
         size :: integer(),
         field_types :: [any()],
         field_offsets :: #{atom() := integer()},
         field_names :: [any()],
         field_defaults :: #{atom() := e_expr()}}).

-record(basic_type,
        {line :: integer(),
         pdepth = 0 :: integer(),
         class :: struct | integer | float | void | any,
         tag :: atom()}).

-record(array_type,
        {line :: integer(),
         elemtype :: e_type(),
         len :: integer()}).

-record(fun_type,
        {line :: integer(),
         params :: [e_type()],
         ret :: e_type()}).

-record(struct_init,
        {line :: integer(),
         name :: atom(),
         field_names :: [any()],
         field_values :: #{atom() := e_expr()}}).

-record(array_init,
        {line :: integer(), elements :: [e_expr()]}).

-record(struct_init_raw,
        {line :: integer(),
         name :: atom(),
         fields :: [e_expr()]}).

-record(function_raw,
        {line :: integer(),
         name :: atom(),
         params :: [e_expr()],
         ret :: e_type(),
         exprs :: [e_expr()]}).

-record(struct_raw,
        {line :: integer(),
         name :: atom(),
         fields :: [e_expr()]}).

-record(return, {line :: integer(), expr :: e_expr()}).

-record(while_expr,
        {line :: integer(),
         condition :: e_expr(),
         exprs :: [e_expr()]}).

-record(if_expr,
        {line :: integer(),
         condition :: e_expr(),
         then :: [e_expr()],
         else :: [e_expr()]}).

-record(const,
        {line :: integer(), name :: atom(), val :: any()}).

-record(vardef,
        {line :: integer(),
         name :: atom(),
         type :: e_type(),
         initval = none :: any()}).

-record(varref, {line :: integer(), name :: atom()}).

-record(integer, {line :: integer(), val :: integer()}).

-record(float, {line :: integer(), val :: float()}).

-record(string, {line :: integer(), val :: string()}).

-record(op1,
        {line :: integer(),
         operator :: atom(),
         operand :: e_expr()}).

-record(op2,
        {line :: integer(),
         operator :: atom(),
         op1 :: e_expr(),
         op2 :: e_expr()}).

-record(call,
        {line :: integer(),
         fn :: e_expr(),
         args :: [e_expr()]}).

-record(sizeof, {line :: integer(), type :: e_type()}).

-record(label, {line :: integer(), name :: atom()}).

-record(goto, {line :: integer(), expr :: e_expr()}).

% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type e_type() :: any().

-type e_expr() :: any().

