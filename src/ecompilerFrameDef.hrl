-record(fun_type,
        {line = 0 :: integer(),
         params = [] :: [eType()],
         ret :: eType()}).

-record(function,
        {line = 0 :: integer(),
         name :: atom(),
         type :: #fun_type{},
         param_names = [] :: [atom()],
         var_types :: variableTypeMap(),
         labels = [] :: [eExpression()],
         exprs = [] :: [eAST()]}).

-record(struct,
        {line = 0 :: integer(),
         name :: atom(),
         size = -1 :: integer(),
         field_types :: variableTypeMap(),
         field_offsets = #{} :: #{atom() := integer()},
         field_names = [] :: [any()],
         field_defaults = #{} :: #{atom() := eExpression()}}).

-record(basic_type,
        {line = 0 :: integer(),
         pdepth = 0 :: integer(),
         class :: struct | integer | float | void | any,
         tag :: atom()}).

-record(array_type,
        {line = 0 :: integer(),
        elemtype :: eType(),
        len :: integer()}).

-record(struct_init,
        {line = 0 :: integer(),
         name :: atom(),
         field_names = [] :: [any()],
         field_values = #{} :: #{atom() := eExpression()}}).

-record(array_init,
        {line = 0 :: integer(),
         elements = [] :: [eExpression()]}).

-record(struct_init_raw,
        {line = 0 :: integer(),
         name :: atom(),
         fields = [] :: [eExpression()]}).

-record(function_raw,
        {line = 0 :: integer(),
         name :: atom(),
         params = [] :: [eExpression()],
         ret :: eType(),
         exprs = [] :: [eExpression()]}).

-record(struct_raw,
        {line = 0 :: integer(),
         name :: atom(),
         fields = [] :: [eExpression()]}).

-record(return,
        {line = 0 :: integer(),
         expr :: eExpression()}).

-record(while_expr,
        {line = 0 :: integer(),
         condition :: eExpression(),
         exprs = [] :: [eExpression()]}).

-record(if_expr,
        {line = 0 :: integer(),
         condition :: eExpression(),
         then = [] :: [eExpression()],
         else = [] :: [eExpression()]}).

-record(const,
        {line = 0 :: integer(),
         name :: atom(),
         val :: any()}).

-record(vardef,
        {line = 0 :: integer(),
         name :: atom(),
         type :: eType(),
         initval = none :: any()}).

-record(varref,
        {line = 0 :: integer(),
         name :: atom()}).

-record(integer,
        {line = 0 :: integer(),
         val :: integer()}).

-record(float,
        {line = 0 :: integer(),
         val :: float()}).

-record(string,
        {line = 0 :: integer(),
         val :: string()}).

-record(op1,
        {line = 0 :: integer(),
         operator :: atom(),
         operand :: eExpression()}).

-record(op2,
        {line = 0 :: integer(),
         operator :: atom(),
         op1 :: eExpression(),
         op2 :: eExpression()}).

-record(call,
        {line = 0 :: integer(),
         fn :: eExpression(),
         args = [] :: [eExpression()]}).

-record(sizeof,
        {line = 0 :: integer(),
         type :: eType()}).

-record(label,
        {line = 0 :: integer(),
         name :: atom()}).

-record(goto,
        {line = 0 :: integer(),
         expr :: eExpression()}).

% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type eType() :: any().

-type eExpression() :: any().

-type structTypeMap() :: #{atom() := #struct{}}.

-type functionTypeMap() :: #{atom() := #fun_type{}}.

-type functionReturnTypeMap() :: #{atom() := eType()}.

-type variableTypeMap() :: #{atom() := eType()}.

-type eAST() :: [eExpression()].

-type compilePassCtx1() :: {structTypeMap(), non_neg_integer()}.
