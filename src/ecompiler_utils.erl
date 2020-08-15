-module(ecompiler_utils).

-export([flat_format/2]).

flat_format(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).

