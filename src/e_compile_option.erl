-module(e_compile_option).
-export([combine/1, default/0]).
-export_type([option/0]).

-type option() :: #{
                    code_pos => non_neg_integer(),
                    data_pos => non_neg_integer(),
                    data_size => non_neg_integer(),
                    ivec_pos => non_neg_integer(),
                    ivec_size => non_neg_integer(),
                    ivec_init_jump => boolean(),
                    prefer_shift => boolean(),
                    entry_function => atom(),
                    wordsize => pos_integer()
                   }.

-spec check(option()) -> option().
check(#{ivec_pos := Vp}) when Vp rem 4 =/= 0 ->
    throw(e_util:fmt("Compile option error: interrupt vector address (~w) is not 4-byte aligned", [Vp]));
check(#{code_pos := Cp}) when Cp rem 4 =/= 0 ->
    throw(e_util:fmt("Compile option error: code start (~w) is not 4-byte aligned", [Cp]));
check(#{ivec_pos := Vp, ivec_size := Vs, code_pos := Cp}) when Vp =< Cp, Vp + Vs > Cp ->
    throw(e_util:fmt("Compile option error: interrupt vector table (~w-~w) overlapped code start (~w)", [Vp, Vp + Vs, Cp]));
check(#{ivec_init_jump := true, ivec_size := 0}) ->
    throw("Compile option error: init jump shouldn't exist when there is no interrupt vector");
check(Options) ->
    Options.

-spec combine(option()) -> option().
combine(CustomOption) ->
    check(maps:merge(default(), CustomOption)).

-spec default() -> option().
default() ->
    option_for_ch32v307().

-spec option_for_ch32v307() -> option().
option_for_ch32v307() ->
    #{
      code_pos => 0,
      data_pos => 16#20000000,
      data_size => 256,
      ivec_pos => 0,
      ivec_size => 0,
      ivec_init_jump => false,
      prefer_shift => false,
      entry_function => main,
      wordsize => 4
     }.
