%-type e_type() :: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64 | f32 | f64 | any.
-type e_type() :: any().

-type e_kvpair() :: {string(), e_type()}.

-type e_expr() :: any().

-record(function, {line :: integer(),
		   name :: string(),
		   params :: [e_kvpair()],
		   ret :: e_type(),
		   exprs :: [e_expr()]}).

-record(while_expr, {line :: integer(),
		     condition :: e_expr(),
		     exprs :: [e_expr()]}).

-record(if_expr, {line :: integer(),
		  condition :: e_expr(),
		  then :: [e_expr()],
		  else :: [e_expr()]}).

-record(struct, {line :: integer(),
		 name :: atom(),
		 fields :: [e_kvpair()]}).

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

