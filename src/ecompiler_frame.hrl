-type e_type() :: 'U8' | 'I8' | 'U16' | 'I16' | 'U32' | 'I32' | 'U64' | 'I64'
    | 'Float64' | 'Float32'.

-type e_kvpair() :: {string(), e_type()}.

-record(box, {line :: integer(), name :: string(), fields :: [e_kvpair()]}).

-record(var, {line :: integer(), name :: string()}).

-record(integer, {line :: integer(), val :: integer()}).

-record(float, {line :: integer(), val :: float()}).

-record(string, {line :: integer(), val :: string()}).

