-module(e_compile_option).
-export([combine/1, default/0, init_jump_pos/1, code_start_pos/1]).
-export_type([option/0]).

-type option() ::
	#{
	init_jump_pos		:= non_neg_integer(),
	isr_vector_pos		:= non_neg_integer(),
	isr_vector_size		:= non_neg_integer(),
	code_start_pos		:= non_neg_integer(),
	ram_start_pos		:= non_neg_integer(),
	ram_end_pos		:= non_neg_integer(),
	entry_function		:= atom(),
	wordsize		:= pos_integer()
	}.

-define(OVERLAPPED(P1, S1, P2), ((P2 >= P1) and (P2 < P1 + S1))).

init_jump_pos(#{init_jump_pos := Jp}) when Jp rem 4 =/= 0 ->
	throw(e_util:fmt("compile option error: init jump position (~w) is not 4-byte aligned", [Jp]));
init_jump_pos(#{init_jump_pos := Jp, isr_vector_pos := Vp}) when Jp > Vp ->
	throw("compile option error: init jump position after isr vector is not supported yet");
init_jump_pos(#{init_jump_pos := Jp}) ->
	Jp.

code_start_pos(#{code_start_pos := Cp}) when Cp rem 4 =/= 0 ->
	throw(e_util:fmt("compile option error: code start (~w) is not 4-byte aligned", [Cp]));
code_start_pos(#{isr_vector_pos := Vp, code_start_pos := Cp}) when Vp > Cp ->
	throw("compile option error: code before isr vector is not supported yet");
code_start_pos(#{isr_vector_pos := Vp, isr_vector_size := Vs, code_start_pos := Cp}) when Vp =< Cp, Vp + Vs > Cp ->
	throw(e_util:fmt("compile option error: isr vector (~w-~w) overlapped code start (~w)", [Vp, Vp + Vs, Cp]));
code_start_pos(#{code_start_pos := Cp}) ->
	Cp.

-spec combine(option()) -> option().
combine(CustomOption) ->
	maps:merge(default(), CustomOption).

-spec default() -> option().
default() ->
	option_for_ch32v307().

-spec option_for_ch32v307() -> option().
option_for_ch32v307() ->
	#{
	init_jump_pos		=> 0,
	isr_vector_pos		=> 0,
	isr_vector_size		=> 416,
	code_start_pos		=> 416,
	ram_start_pos		=> 16#20000000,
	ram_end_pos		=> 16#20010000,
	entry_function		=> main,
	wordsize		=> 4
	}.

