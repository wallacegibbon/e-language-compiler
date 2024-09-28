-module(e_compile_option).
-export([combine/1, default/0, code_start_pos/1]).
-export_type([option/0]).

-type option() ::
	#{
	isr_vector_pos => non_neg_integer(),
	isr_vector_size => non_neg_integer(),
	init_code_pos => non_neg_integer(),
	entry_function => atom(),
	wordsize => pos_integer()
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
	#{
	isr_vector_pos => 0,
	isr_vector_size => 416,
	init_code_pos => 0,
	entry_function => main,
	wordsize => 4
	}.

