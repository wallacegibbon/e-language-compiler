%-type e_type() :: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64 | f32 | f64 | any.

-type e_kvpair() :: {string(), any()}.

-record(struct, {line :: integer(), name :: string(), fields :: [e_kvpair()]}).

-record(var, {line :: integer(), name :: string()}).

-record(integer, {line :: integer(), val :: integer()}).

-record(float, {line :: integer(), val :: float()}).

-record(string, {line :: integer(), val :: string()}).

