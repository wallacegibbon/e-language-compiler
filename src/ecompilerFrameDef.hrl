-record(functionType,
        {line = 0 :: integer(),
         parameters = [] :: [eType()],
         ret :: eType()}).

-record(function,
        {line = 0 :: integer(),
         name :: atom(),
         type :: #functionType{},
         parameterNames = [] :: [atom()],
         variableTypeMap :: variableTypeMap(),
         labels = [] :: [eExpression()],
         statements = [] :: [eAST()]}).

-record(struct,
        {line = 0 :: integer(),
         name :: atom(),
         size = -1 :: integer(),
         fieldTypeMap :: variableTypeMap(),
         fieldOffsetMap = #{} :: #{atom() := integer()},
         fieldNames = [] :: [any()],
         fieldDefaultValueMap = #{} :: #{atom() := eExpression()}}).

-record(basicType,
        {line = 0 :: integer(),
         pdepth = 0 :: integer(),
         class :: struct | integer | float | void | any,
         tag :: atom()}).

-record(arrayType,
        {line = 0 :: integer(),
         elemtype :: eType(),
         length :: integer()}).

-record(structInitializeExpressionRaw,
        {line = 0 :: integer(),
         name :: atom(),
         fields = [] :: [eExpression()]}).

-record(structInitializeExpression,
        {line = 0 :: integer(),
         name :: atom(),
         fieldNames = [] :: [any()],
         fieldValueMap = #{} :: #{atom() := eExpression()}}).

-record(arrayInitializeExpression,
        {line = 0 :: integer(),
         elements = [] :: [eExpression()]}).

-record(functionRaw,
        {line = 0 :: integer(),
         name :: atom(),
         parameters = [] :: [eExpression()],
         returnType :: eType(),
         statements = [] :: [eExpression()]}).

-record(structRaw,
        {line = 0 :: integer(),
         name :: atom(),
         fields = [] :: [eExpression()]}).

-record(returnStatement,
        {line = 0 :: integer(),
         expression :: eExpression()}).

-record(whileStatement,
        {line = 0 :: integer(),
         condition :: eExpression(),
         statements = [] :: [eExpression()]}).

-record(ifStatement,
        {line = 0 :: integer(),
         condition :: eExpression(),
         then = [] :: [eExpression()],
         else = [] :: [eExpression()]}).

-record(variableDefinition,
        {line = 0 :: integer(),
         name :: atom(),
         type :: eType(),
         initialValue = none :: any()}).

-record(variableReference,
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

-record(operatorExpression1,
        {line = 0 :: integer(),
         operator :: atom(),
         operand :: eExpression()}).

-record(operatorExpression2,
        {line = 0 :: integer(),
         operator :: atom(),
         operand1 :: eExpression(),
         operand2 :: eExpression()}).

-record(callExpression,
        {line = 0 :: integer(),
         fn :: eExpression(),
         args = [] :: [eExpression()]}).

-record(sizeofExpression,
        {line = 0 :: integer(),
         type :: eType()}).

-record(gotoLabel,
        {line = 0 :: integer(),
         name :: atom()}).

-record(gotoStatement,
        {line = 0 :: integer(),
         expression :: eExpression()}).

% primitive types: u8|i8|u16|i16|u32|i32|u64|i64|f64|f32|void|any.
-type eType() :: #basicType{} | #arrayType{} | #functionType{} | any().

-type eStatement() :: #ifStatement{} | #whileStatement{} | #gotoStatement{} | #returnStatement{}.
-type eExpression() :: #operatorExpression1{} | #operatorExpression2{} | #callExpression{} | #sizeofExpression{} | any().

-type structTypeMap() :: #{atom() := #struct{}}.

-type functionTypeMap() :: #{atom() := #functionType{}}.

-type functionReturnTypeMap() :: #{atom() := eType()}.

-type variableTypeMap() :: #{atom() := eType()}.

-type eAST() :: [eExpression()].

-type compilePassCtx1() :: {structTypeMap(), non_neg_integer()}.
