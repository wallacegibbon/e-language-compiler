-type e_type() :: u8 | i8 | u16 | i16 | u32 | i32 | u64 | i64
    | float64 | float32.

-type e_kvpair() :: {string(), e_type()}.

-record(box, {line :: integer(), name :: string(), fields :: [e_kvpair()]}).

-record(var, {line :: integer(), name :: string()}).

-record(integer, {line :: integer(), val :: integer()}).

-record(float, {line :: integer(), val :: float()}).

-record(string, {line :: integer(), val :: string()}).

