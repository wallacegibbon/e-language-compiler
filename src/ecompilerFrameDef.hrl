-record(fun_type,                   {   line                            :: integer(),
                                        params                          :: [eType()],
                                        ret                             :: eType()}).

-record(function,                   {   line                            :: integer(),
                                        name                            :: atom(),
                                        type                            :: #fun_type{},
                                        param_names                     :: [atom()],
                                        var_types                       :: variableTypeMap(),
                                        labels                          :: [eExpression()],
                                        exprs                           :: [eAST()]}).

-record(struct,                     {   line                            :: integer(),
                                        name                            :: atom(),
                                        size                            :: integer(),
                                        field_types                     :: variableTypeMap(),
                                        field_offsets                   :: #{atom() := integer()},
                                        field_names                     :: [any()],
                                        field_defaults                  :: #{atom() := eExpression()}}).

-record(basic_type,                 {   line                            :: integer(),
                                        pdepth = 0                      :: integer(),
                                        class                           :: struct | integer | float | void | any,
                                        tag                             :: atom()}).

-record(array_type,                 {   line                            :: integer(),
                                        elemtype                        :: eType(),
                                        len                             :: integer()}).

-record(struct_init,                {   line                            :: integer(),
                                        name                            :: atom(),
                                        field_names                     :: [any()],
                                        field_values                    :: #{atom() := eExpression()}}).

-record(array_init,                 {   line                            :: integer(),
                                        elements                        :: [eExpression()]}).

-record(struct_init_raw,            {   line                            :: integer(),
                                        name                            :: atom(),
                                        fields                          :: [eExpression()]}).

-record(function_raw,               {   line                            :: integer(),
                                        name                            :: atom(),
                                        params                          :: [eExpression()],
                                        ret                             :: eType(),
                                        exprs                           :: [eExpression()]}).

-record(struct_raw,                 {   line                            :: integer(),
                                        name                            :: atom(),
                                        fields                          :: [eExpression()]}).

-record(return,                     {   line                            :: integer(),
                                        expr                            :: eExpression()}).

-record(while_expr,                 {   line                            :: integer(),
                                        condition                       :: eExpression(),
                                        exprs                           :: [eExpression()]}).

-record(if_expr,                    {   line                            :: integer(),
                                        condition                       :: eExpression(),
                                        then                            :: [eExpression()],
                                        else                            :: [eExpression()]}).

-record(const,                      {   line                            :: integer(),
                                        name                            :: atom(),
                                        val                             :: any()}).

-record(vardef,                     {   line                            :: integer(),
                                        name                            :: atom(),
                                        type                            :: eType(),
                                        initval = none                  :: any()}).

-record(varref,                     {   line                            :: integer(),
                                        name                            :: atom()}).

-record(integer,                    {   line                            :: integer(),
                                        val                             :: integer()}).

-record(float,                      {   line                            :: integer(),
                                        val                             :: float()}).

-record(string,                     {   line                            :: integer(),
                                        val                             :: string()}).

-record(op1,                        {   line                            :: integer(),
                                        operator                        :: atom(),
                                        operand                         :: eExpression()}).

-record(op2,                        {   line                            :: integer(),
                                        operator                        :: atom(),
                                        op1                             :: eExpression(),
                                        op2                             :: eExpression()}).

-record(call,                       {   line                            :: integer(),
                                        fn                              :: eExpression(),
                                        args                            :: [eExpression()]}).

-record(sizeof,                     {   line                            :: integer(),
                                        type                            :: eType()}).

-record(label,                      {   line                            :: integer(),
                                        name                            :: atom()}).

-record(goto,                       {   line                            :: integer(),
                                        expr                            :: eExpression()}).

% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type eType()                                                           :: any().
-type eExpression()                                                     :: any().

-type structTypeMap()                                                   :: #{atom() := #struct{}}.
-type functionTypeMap()                                                 :: #{atom() := #fun_type{}}.
-type functionReturnTypeMap()                                           :: #{atom() := eType()}.
-type variableTypeMap()                                                 :: #{atom() := eType()}.
-type eAST()                                                            :: [eExpression()].

-type compilePassCtx1()                                                 :: {#struct{}, functionTypeMap()}.