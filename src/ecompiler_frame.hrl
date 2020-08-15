%-type e_type() :: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64 | f32 | f64 | any.
-type e_type() :: any().
-type e_expr() :: any().

-record(function_1, {name :: atom(),
		     params :: [atom()],
		     params_defaultinit :: [e_expr()],
		     ret :: e_type(),
		     vars :: #{atom() => e_type()},
		     exprs :: [e_expr()]}).

-record(struct_1, {name :: atom(),
		   fields :: [any()],
		   initcode :: [e_expr()]}).

-record(function, {line :: integer(),
		   name :: atom(),
		   params :: [e_expr()],
		   ret :: e_type(),
		   exprs :: [e_expr()]}).

-record(return, {line :: integer(),
		 expr :: e_expr()}).

-record(while_expr, {line :: integer(),
		     condition :: e_expr(),
		     exprs :: [e_expr()]}).

-record(if_expr, {line :: integer(),
		  condition :: e_expr(),
		  then :: [e_expr()],
		  else :: [e_expr()]}).

-record(struct, {line :: integer(),
		 name :: atom(),
		 fields :: [e_expr()]}).

-record(const, {line :: integer(),
		name :: atom(),
		val :: any()}).

-record(vardef, {line :: integer(),
		 name :: atom(),
		 type :: e_type(),
		 initval = none :: any()}).

-record(varref, {line :: integer(),
		 name :: atom()}).

-record(integer, {line :: integer(),
		  val :: integer()}).

-record(float, {line :: integer(),
		val :: float()}).

-record(string, {line :: integer(),
		 val :: string()}).

-record(op1, {line :: integer(),
	      operator :: atom(),
	      operand :: e_expr()}).

-record(op2, {line :: integer(),
	      operator :: atom(),
	      op1 :: e_expr(),
	      op2 :: e_expr()}).

-record(call, {line :: integer(),
	       name :: atom(),
	       args :: [e_expr()]}).

-record(basic_type, {line :: integer(),
		     type :: e_type()}).

-record(box_type, {line :: integer(),
		   elemtype :: e_type(),
		   size :: integer()}).

