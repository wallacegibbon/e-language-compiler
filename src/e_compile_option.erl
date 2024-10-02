-module(e_compile_option).
-export([combine/1, default/0, code_start_pos/1]).
-export_type([option/0]).

-type option() ::
	#{
	isr_vector_pos		:= non_neg_integer(),
	isr_vector_size		:= non_neg_integer(),
	init_code_pos		:= non_neg_integer(),
	ram_start_pos		:= non_neg_integer(),
	ram_end_pos		:= non_neg_integer(),
	entry_function		:= atom(),
	wordsize		:= pos_integer()
	}.

code_start_pos(#{isr_vector_pos := Pos, isr_vector_size := Size, init_code_pos := InitPos}) ->
	AfterVector = Pos + Size,
	case AfterVector > InitPos of
		true ->
			AfterVector;
		false ->
			InitPos
	end.

-spec combine(option()) -> option().
combine(CustomOption) ->
	maps:merge(default(), CustomOption).

-spec default() -> option().
default() ->
	option_for_ch32v307().

-spec option_for_ch32v307() -> option().
option_for_ch32v307() ->
	#{
	isr_vector_pos		=> 0,
	isr_vector_size		=> 416,
	%isr_vector_pos		=> 16,
	%isr_vector_size		=> 256,
	init_code_pos		=> 0,
	ram_start_pos		=> 16#20000000,
	ram_end_pos		=> 16#20010000,
	entry_function		=> main,
	wordsize		=> 4
	}.

