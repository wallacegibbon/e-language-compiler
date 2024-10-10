-module(e_compile_option).
-export([combine/1, default/0]).
-export_type([option/0]).

-type option() ::
	#{
	init_jump_pos		=> non_neg_integer(),
	isr_vector_pos		=> non_neg_integer(),
	isr_vector_size		=> non_neg_integer(),
	code_pos		=> non_neg_integer(),
	data_pos		=> non_neg_integer(),
	data_size		=> non_neg_integer(),
	entry_function		=> atom(),
	wordsize		=> pos_integer()
	}.

-spec check(option()) -> option().
check(#{code_pos := Cp}) when Cp rem 4 =/= 0 ->
	throw(e_util:fmt("compile option error: code start (~w) is not 4-byte aligned", [Cp]));
check(#{isr_vector_pos := Vp, code_pos := Cp}) when Vp > Cp ->
	throw("compile option error: code before isr vector is not supported yet");
check(#{isr_vector_pos := Vp, isr_vector_size := Vs, code_pos := Cp}) when Vp =< Cp, Vp + Vs > Cp ->
	throw(e_util:fmt("compile option error: isr vector (~w-~w) overlapped code start (~w)", [Vp, Vp + Vs, Cp]));
check(#{init_jump_pos := Jp}) when Jp rem 4 =/= 0 ->
	throw(e_util:fmt("compile option error: init jump position (~w) is not 4-byte aligned", [Jp]));
check(#{init_jump_pos := Jp, isr_vector_pos := Vp}) when Jp > Vp ->
	throw("compile option error: init jump position after isr vector is not supported yet");
check(#{init_jump_pos := _, isr_vector_size := 0}) ->
	throw("compile option error: init jump shouldn't exist when there are no isr vector");
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
	%init_jump_pos		=> 0,
	isr_vector_pos		=> 0,
	isr_vector_size		=> 0,
	code_pos		=> 0,
	data_pos		=> 16#20000000,
	data_size		=> 16#10000,
	entry_function		=> main,
	wordsize		=> 4
	}.

