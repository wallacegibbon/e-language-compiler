-module(e_dumper_machine1).
-export([generate_code/4]).

-type scan_context() ::
	#{
	label_map		:= #{atom() => non_neg_integer()},
	offset_map		:= #{non_neg_integer() => [atom()]},
	wordsize		:= non_neg_integer()
	}.

-spec generate_code([tuple()], string(), #{non_neg_integer() => atom()}, e_compile_option:option()) -> ok.
generate_code(IRs, OutputFile, InterruptMap, Options) ->
	#{wordsize := WordSize, isr_vector_pos := ISR_Pos, isr_vector_size := ISR_Size} = Options,
	CodePos = e_compile_option:code_start_pos(Options),
	ScanContext = #{label_map => #{}, offset_map => #{}, wordsize => WordSize},
	{Instrs, #{label_map := LabelMap, offset_map := OffsetMap}} = scan_address(IRs, CodePos, [], ScanContext),
	Instructions0 = lists:map(fun encode_instr/1, replace_address(Instrs, LabelMap)),
	{ok, DefaultISRAddr} = maps:find('__default_isr', LabelMap),
	ISR_skip = isr_vector_skip(Options),
	VectorTable = generate_isr_vector_table(ISR_skip, ISR_Pos, ISR_Size, WordSize, [], DefaultISRAddr, InterruptMap, LabelMap),
	Instructions1 = lists:reverse(VectorTable, Instructions0),
	{ok, StartAddress} = maps:find('__init', LabelMap),
	{InitJumpIRs, BinStartPos} = generate_init_jump(StartAddress, Options),
	Instructions = InitJumpIRs ++ Instructions1,
	Fn1 = fun(IO_Dev) -> write_binary(Instructions, BinStartPos, IO_Dev) end,
	e_util:file_write(OutputFile, Fn1),
	Fn2 = fun(IO_Dev) -> write_detail(Instructions, BinStartPos, OffsetMap, IO_Dev) end,
	e_util:file_write(OutputFile ++ ".detail", Fn2),
	ok.

-spec generate_init_jump(non_neg_integer(), e_compile_option:option()) -> {[tuple()], non_neg_integer()}.
generate_init_jump(StartAddress, #{isr_vector_pos := ISR_Pos} = Options) ->
	case e_compile_option:init_jump_pos(Options) of
		{ok, InitJumpPos} ->
			{[encode_instr({{j, StartAddress - InitJumpPos}, InitJumpPos})], InitJumpPos};
		none ->
			{[], ISR_Pos}
	end.

%% On some platforms, we skip the first 4-byte (reserved to an init jump instruction) on isr vector generating.
-spec isr_vector_skip(e_compile_option:option()) -> non_neg_integer().
isr_vector_skip(#{init_jump_exist := false})	-> 0;
isr_vector_skip(#{init_jump_exist := true})	-> 4.

generate_isr_vector_table(N, Pos, Size, WordSize, R, Default, InterruptMap, LabelMap) when N < Size ->
	ISR_Addr = get_isr_address(N div WordSize, Default, InterruptMap, LabelMap),
	NewItem = {word, <<ISR_Addr:32/little>>, N + Pos},
	generate_isr_vector_table(N + WordSize, Pos, Size, WordSize, [NewItem | R], Default, InterruptMap, LabelMap);
generate_isr_vector_table(Size, _, Size, _, R, _, _, _) ->
	R.

get_isr_address(InterruptID, DefaultISRAddr, InterruptMap, LabelMap) ->
	case maps:find(InterruptID, InterruptMap) of
		{ok, Label} ->
			maps:get(Label, LabelMap);
		_ ->
			DefaultISRAddr
	end.

-spec scan_address([tuple()], non_neg_integer(), [tuple()], scan_context()) -> {R, scan_context()} when R :: [tuple()].
scan_address([{label, {align, N}, Name} | Rest], Offset, Result, #{label_map := LabelMap, offset_map := OffsetMap} = Ctx) ->
	FixedOffset = e_util:fill_unit_pessi(Offset, N),
	NewLabelMap = LabelMap#{Name => FixedOffset},
	NewOffsetMap = maps:update_with(FixedOffset, fun(Ns) -> [Name | Ns] end, [Name], OffsetMap),
	scan_address(Rest, FixedOffset, Result, Ctx#{label_map := NewLabelMap, offset_map := NewOffsetMap});
scan_address([{fn, Name} | Rest], Offset, Result, #{wordsize := WordSize} = Ctx) ->
	scan_address([{label, {align, WordSize}, Name} | Rest], Offset, Result, Ctx);
scan_address([{la, _, _} = Orig | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + 8, [{Orig, Offset} | Result], Ctx);
scan_address([{string, _, Length} = Orig | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + Length + 1, [{Orig, Offset} | Result], Ctx);
scan_address([Any | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + 4, [{Any, Offset} | Result], Ctx);
scan_address([], _, Result, Ctx) ->
	{lists:reverse(Result), Ctx}.

-define(IS_BR_INSTR(Tag),
	(
	Tag =:= beq orelse Tag =:= bne orelse Tag =:= bge orelse Tag =:= blt orelse Tag =:= bgt orelse Tag =:= ble
	)).

replace_address([{{Br, R1, R2, Label}, Offset} | Rest], LabelMap) when ?IS_BR_INSTR(Br) ->
	{ok, Address} = maps:find(Label, LabelMap),
	RelativeOffset = Address - Offset,
	if
		RelativeOffset > 4095 orelse RelativeOffset < -4096 ->
			throw({instruction_error, Br, {out_of_range, RelativeOffset}});
		true ->
			[{{Br, R1, R2, RelativeOffset}, Offset} | replace_address(Rest, LabelMap)]
	end;
replace_address([{{j, Label}, Offset} | Rest], LabelMap) ->
	{ok, Address} = maps:find(Label, LabelMap),
	RelativeOffset = Address - Offset,
	if
		RelativeOffset > 16#7FFFF orelse RelativeOffset < -16#80000 ->
			throw({instruction_error, j, {out_of_range, RelativeOffset}});
		true ->
			[{{j, RelativeOffset}, Offset} | replace_address(Rest, LabelMap)]
	end;
replace_address([{{la, R, Label}, Offset} | Rest], LabelMap) ->
	{ok, Address} = maps:find(Label, LabelMap),
	{High, Low} = e_util:u_type_immedi(Address - Offset),
	[{{auipc, R, High}, Offset}, {{addi, R, R, Low}, Offset + 4} | replace_address(Rest, LabelMap)];
replace_address([Any | Rest], LabelMap) ->
	[Any | replace_address(Rest, LabelMap)];
replace_address([], _) ->
	[].

-define(IS_IMMEDI_CALC(Tag),
	(
	Tag =:= addi orelse Tag =:= andi orelse Tag =:= ori orelse Tag =:= xori orelse
	Tag =:= slli orelse Tag =:= srai
	)).

-define(IS_NORMAL_CALC(Tag),
	(
	Tag =:= add orelse Tag =:= sub orelse Tag =:= 'and' orelse Tag =:= 'or' orelse Tag =:= 'xor' orelse
	Tag =:= sll orelse Tag =:= sra orelse
	Tag =:= mul orelse Tag =:= mulh orelse Tag =:= 'div' orelse Tag =:= 'rem'
	)).

encode_instr({{Br, {x, N1}, {x, N2}, Address} = I, Offset}) when Br =:= bge; Br =:= blt; Br =:= beq; Br =:= bne ->
	{High, Low} = e_util:b_type_immedi(Address),
	Immedi = (High bsl 25) bor (Low bsl 7),
	Code =  Immedi bor (N2 bsl 20) bor (N1 bsl 15) bor (f3code_of(Br) bsl 12) bor 2#1100011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{S, {x, N1}, {{x, N2}, O}} = I, Offset}) when S =:= sw; S =:= sb ->
	{High, Low} = e_util:s_type_immedi(O),
	Immedi = (High bsl 25) bor (Low bsl 7),
	%% N2 is RS1, the pointer.
	Code = Immedi bor (N2 bsl 15) bor (N1 bsl 20) bor (f3code_of(S) bsl 12) bor 2#0100011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{L, {x, N1}, {{x, N2}, O}} = I, Offset}) when L =:= lw; L =:= lb ->
	Code = (O bsl 20)  bor (N2 bsl 15) bor (f3code_of(L) bsl 12) bor (N1 bsl 7) bor 2#0000011,
	{I, <<Code:32/little>>, Offset};
%% SRAI is special, it's like `I` and `R` at the same time. both `f7` and immediate exists.
encode_instr({{srai, {x, N1}, {x, N2}, A} = I, Offset}) ->
	FixedA = A bor 2#010000000000,
	Code = (FixedA bsl 20) bor (N2 bsl 15) bor (f3code_of(srai) bsl 12) bor (N1 bsl 7) bor 2#0010011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{Tag, {x, N1}, {x, N2}, A} = I, Offset}) when ?IS_IMMEDI_CALC(Tag) ->
	Code = (A bsl 20) bor (N2 bsl 15) bor (f3code_of(Tag) bsl 12) bor (N1 bsl 7) bor 2#0010011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{Tag, {x, N1}, {x, N2}, CSR} = I, Offset}) when Tag =:= csrrw; Tag =:= csrrs; Tag =:= csrrc ->
	Code = (CSR bsl 20) bor (N2 bsl 15) bor (f3code_of(Tag) bsl 12) bor (N1 bsl 7) bor 2#1110011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{Tag, {x, N1}, {x, N2}, {x, N3}} = I, Offset}) when ?IS_NORMAL_CALC(Tag) ->
	Rs = (N3 bsl 20) bor (N2 bsl 15) bor (N1 bsl 7),
	Code = Rs bor (f7code_of(Tag) bsl 25) bor (f3code_of(Tag) bsl 12) bor 2#0110011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{auipc, {x, N}, Address} = I, Offset}) ->
	Code = (Address bsl 12) bor (N bsl 7) bor 2#0010111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{lui, {x, N}, Address} = I, Offset}) ->
	Code = (Address bsl 12) bor (N bsl 7) bor 2#0110111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{j, Address} = I, Offset}) ->
	Code = (e_util:j_type_immedi(Address) bsl 12) bor 2#1101111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{jalr, {x, N1}, {x, N2}} = I, Offset}) ->
	Code = (N2 bsl 15) bor (N1 bsl 7) bor 2#1100111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{mret} = I, Offset}) ->
	{I, <<16#30200073:32/little>>, Offset};
encode_instr({{wfi} = I, Offset}) ->
	{I, <<16#10500073:32/little>>, Offset};
encode_instr({{string, _, _} = I, Offset}) ->
	{I, <<>>, Offset};
encode_instr(Any) ->
	Any.

write_detail([{{string, Content, Length}, _, Loc} | Rest], Loc, OffsetMap, IO_Dev) ->
	io:format(IO_Dev, "~8.16.0b:\t\t\t\t\"~s\\0\"~n", [Loc, e_util:fix_special_chars(Content)]),
	write_detail(Rest, Loc + Length + 1, OffsetMap, IO_Dev);
write_detail([{word, Raw, Loc} | Rest], Loc, OffsetMap, IO_Dev) ->
	io:format(IO_Dev, "~8.16.0b:\t~s~n", [Loc, fmt_code(Raw)]),
	write_detail(Rest, Loc + byte_size(Raw), OffsetMap, IO_Dev);
write_detail([{Instr, Raw, Loc} | Rest], Loc, OffsetMap, IO_Dev) ->
	case maps:find(Loc, OffsetMap) of
		{ok, Labels} ->
			lists:foreach(fun(L) -> io:format(IO_Dev, "\t\t\t\t~s:~n", [L]) end, Labels);
		_ ->
			ok
	end,
	io:format(IO_Dev, "~8.16.0b:\t~s\t\t~w~n", [Loc, fmt_code(Raw), Instr]),
	write_detail(Rest, Loc + byte_size(Raw), OffsetMap, IO_Dev);
write_detail([{_, _, Loc} | _] = Data, N, OffsetMap, IO_Dev) when Loc > N ->
	io:format(IO_Dev, "~8.16.0b:\t      00~n", [N]),
	write_detail(Data, N + 1, OffsetMap, IO_Dev);
write_detail([], _, _, _) ->
	ok.

fmt_code(<<A, B, C, D>>) ->
	io_lib:format("~2.16.0b~2.16.0b~2.16.0b~2.16.0b", [D, C, B, A]);
fmt_code(<<>>) ->
	"".

write_binary([{{string, Content, Length}, _, Loc} | Rest], Loc, IO_Dev) ->
	file:write(IO_Dev, [Content, 0]),
	write_binary(Rest, Loc + Length + 1, IO_Dev);
write_binary([{_, Raw, Loc} | Rest], Loc, IO_Dev) ->
	file:write(IO_Dev, Raw),
	write_binary(Rest, Loc + byte_size(Raw), IO_Dev);
write_binary([{_, _, Loc} | _] = Data, N, IO_Dev) when Loc > N ->
	file:write(IO_Dev, [0]),
	write_binary(Data, N + 1, IO_Dev);
write_binary([], _, _) ->
	ok.

f3code_of(beq)		-> 2#000;
f3code_of(bne)		-> 2#001;
f3code_of(blt)		-> 2#100;
f3code_of(bge)		-> 2#101;
f3code_of(lw)		-> 2#010;
f3code_of(lb)		-> 2#000;
f3code_of(sw)		-> 2#010;
f3code_of(sb)		-> 2#000;
f3code_of(addi)		-> 2#000;
f3code_of(andi)		-> 2#111;
f3code_of(ori)		-> 2#110;
f3code_of(xori)		-> 2#100;
f3code_of(slli)		-> 2#001;
f3code_of(srai)		-> 2#101;
f3code_of(add)		-> 2#000;
f3code_of(sub)		-> 2#000;
f3code_of('and')	-> 2#111;
f3code_of('or')		-> 2#110;
f3code_of('xor')	-> 2#100;
f3code_of(sll)		-> 2#001;
f3code_of(sra)		-> 2#101;
f3code_of(mul)		-> 2#000;
f3code_of(mulh)		-> 2#001;
f3code_of('div')	-> 2#100;
f3code_of('rem')	-> 2#110;
f3code_of('csrrw')	-> 2#001;
f3code_of('csrrs')	-> 2#010;
f3code_of('csrrc')	-> 2#011.

f7code_of(sub)		-> 2#0100000;
f7code_of(sra)		-> 2#0100000;
f7code_of(mul)		-> 2#0000001;
f7code_of(mulh)		-> 2#0000001;
f7code_of('div')	-> 2#0000001;
f7code_of('rem')	-> 2#0000001;
f7code_of(_)		-> 2#0000000.

