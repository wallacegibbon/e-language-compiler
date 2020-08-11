%-type e_type() :: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64 | f32 | f64 | any.
-type e_type() :: any().

-type e_kvpair() :: {string(), e_type()}.

-type e_expr() :: any().

-record(function, {line :: integer(),
		   name :: string(),
		   args :: [e_kvpair()],
		   ret :: e_type(),
		   exprs :: [e_expr()]}).

-record(struct, {line :: integer(),
		 name :: string(),
		 fields :: [e_kvpair()]}).

-record(var, {line :: integer(), name :: string()}).

-record(integer, {line :: integer(), val :: integer()}).

-record(float, {line :: integer(), val :: float()}).

-record(string, {line :: integer(), val :: string()}).

