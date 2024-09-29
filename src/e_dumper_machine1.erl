-module(e_dumper_machine1).
-export([generate_code/3]).

-type scan_context() ::
	#{
	label_map	=> #{atom() => non_neg_integer()},
	offset_map	=> #{non_neg_integer() => [atom()]},
	wordsize	=> non_neg_integer()
	}.

-spec generate_code([tuple()], string(), e_compile_option:option()) -> ok.
generate_code(IRs, OutputFile, #{wordsize := WordSize} = Options) ->
	StartPos = e_compile_option:code_start_pos(Options),
	ScanContext = #{label_map => #{}, offset_map => #{}, wordsize => WordSize},
	{Instrs, #{label_map := LabelMap, offset_map := OffsetMap}} = scan_address(IRs, StartPos, [], ScanContext),
	%io:format(">> LabelMap:~p~n>> OffsetMap:~p~n", [LabelMap, OffsetMap]),
	Encoded = lists:map(fun encode_instr/1, replace_address(Instrs, LabelMap)),
	Fn1 = fun(IO_Dev) -> write_binary(Encoded, StartPos, IO_Dev) end,
	e_util:file_write(OutputFile, Fn1),
	Fn2 = fun(IO_Dev) -> write_detail(Encoded, StartPos, OffsetMap, IO_Dev) end,
	e_util:file_write(OutputFile ++ ".detail", Fn2),
	ok.

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
	[{{Br, R1, R2, Address - Offset}, Offset} | replace_address(Rest, LabelMap)];
replace_address([{{la, R, Label}, Offset} | Rest], LabelMap) ->
	{ok, Address} = maps:find(Label, LabelMap),
	{High, Low} = e_util:u_type_immedi(Address - Offset),
	[{{auipc, R, High}, Offset}, {{addi, R, R, Low}, Offset + 4} | replace_address(Rest, LabelMap)];
replace_address([{{j, Label}, Offset} | Rest], LabelMap) ->
	{ok, Address} = maps:find(Label, LabelMap),
	if
		Address > 16#7FFFF orelse Address < -16#80000 ->
			throw({instruction_error, j, {out_of_range, Address}});
		true ->
			[{{j, Address - Offset}, Offset} | replace_address(Rest, LabelMap)]
	end;
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
	io:format(IO_Dev, "~8.16.0B:\t\t\t\t\"~s\\0\"~n", [Loc, e_util:fix_special_chars(Content)]),
	write_detail(Rest, Loc + Length + 1, OffsetMap, IO_Dev);
write_detail([{Instr, Raw, Loc} | Rest], Loc, OffsetMap, IO_Dev) ->
	case maps:find(Loc, OffsetMap) of
		{ok, Labels} ->
			lists:foreach(fun(L) -> io:format(IO_Dev, "\t~s:~n", [L]) end, Labels);
		_ ->
			ok
	end,
	io:format(IO_Dev, "~8.16.0B:\t~s\t\t~w~n", [Loc, fmt_code(Raw), Instr]),
	write_detail(Rest, Loc + byte_size(Raw), OffsetMap, IO_Dev);
write_detail([{_, _, Loc} | _] = Data, N, OffsetMap, IO_Dev) when Loc > N ->
	io:format(IO_Dev, "\t\t\t\t\t.byte 0~n", []),
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
f3code_of('rem')	-> 2#110.

f7code_of(sub)		-> 2#0100000;
f7code_of(sra)		-> 2#0100000;
f7code_of(mul)		-> 2#0000001;
f7code_of(mulh)		-> 2#0000001;
f7code_of('div')	-> 2#0000001;
f7code_of('rem')	-> 2#0000001;
f7code_of(_)		-> 2#0000000.

