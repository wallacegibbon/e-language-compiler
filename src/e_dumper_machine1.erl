-module(e_dumper_machine1).
-export([generate_code/2]).

-type scan_context() :: {LabelMap :: #{atom() => non_neg_integer()}, OffsetMap :: #{non_neg_integer() => [atom()]}}.

-spec generate_code([tuple()], string()) -> ok.
generate_code(IRs, OutputFile) ->
	{Instrs, {LabelMap, OffsetMap}} = scan_address(IRs, 0, [], {#{}, #{}}),
	%io:format(">> LabelMap:~p~n>> OffsetMap:~p~n", [LabelMap, OffsetMap]),
	Encoded = lists:map(fun encode_instr/1, replace_address(Instrs, LabelMap)),
	Fn1 = fun(IO_Dev) -> write_binary(Encoded, IO_Dev) end,
	e_util:file_write(OutputFile, Fn1),
	Fn2 = fun(IO_Dev) -> write_detail(Encoded, OffsetMap, IO_Dev) end,
	e_util:file_write(OutputFile ++ ".detail", Fn2),
	ok.

-spec scan_address([tuple()], non_neg_integer(), [tuple()], scan_context()) -> {R, scan_context()} when R :: [tuple()].
scan_address([{L, Name} | Rest], Offset, Result, {LabelMap, OffsetMap}) when L =:= fn; L =:= label ->
	NewLabelMap = LabelMap#{Name => Offset},
	NewOffsetMap = maps:update_with(Offset, fun(Ns) -> [Name | Ns] end, [Name], OffsetMap),
	scan_address(Rest, Offset, Result, {NewLabelMap, NewOffsetMap});
scan_address([{la, _, _} = Orig | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + 8, [{Orig, Offset} | Result], Ctx);
scan_address([{string, Data} = Orig | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + byte_size(Data), [{Orig, Offset} | Result], Ctx);
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
		Address =:= Offset + 4 ->
			replace_address(Rest, LabelMap);
		Address > 16#7FFFF orelse Address < -16#80000 ->
			throw({instruction_error, j, {out_of_range, Address}});
		true ->
			[{{j, Address - Offset}, Offset} | replace_address(Rest, LabelMap)]
	end;
replace_address([Any | Rest], LabelMap) ->
	[Any | replace_address(Rest, LabelMap)];
replace_address([], _) ->
	[].

encode_instr({{Br, {x, N1}, {x, N2}, Address} = I, Offset}) when Br =:= bge; Br =:= blt; Br =:= beq; Br =:= bne ->
	{High, Low} = e_util:b_type_immedi(Address),
	Immedi = (High bsl 24) bor (Low bsl 6),
	Code =  Immedi bor (N2 bsl 19) bor (N1 bsl 14) bor (f3code_of(Br) bsl 11) bor 2#1100011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{S, {x, N1}, {{x, N2}, O}} = I, Offset}) when S =:= sw; S =:= sb ->
	{High, Low} = e_util:s_type_immedi(O),
	Immedi = (High bsl 24) bor (Low bsl 6),
	Code = Immedi bor (N2 bsl 19) bor (N1 bsl 14) bor (f3code_of(S) bsl 11) bor 2#0100011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{L, {x, N1}, {{x, N2}, O}} = I, Offset}) when L =:= lw; L =:= lb ->
	Code = (O bsl 19)  bor (N2 bsl 14) bor (f3code_of(L) bsl 11) bor (N1 bsl 6) bor 2#0000011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{Imm, {x, N1}, {x, N2}, A} = I, Offset}) when Imm =:= addi; Imm =:= andi; Imm =:= ori; Imm =:= xori; Imm =:= slli; Imm =:= srai ->
	Code = (A bsl 19) bor (N2 bsl 14) bor 0 bor (N1 bsl 6) bor 2#0010011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{Tag, {x, N1}, {x, N2}, {x, N3}} = I, Offset}) when Tag =:= add; Tag =:= sub; Tag =:= 'and'; Tag =:= 'or'; Tag =:= 'xor'; Tag =:= sll; Tag =:= sra ->
	Rs = (N3 bsl 19) bor (N2 bsl 14) bor (N1 bsl 6),
	Code = Rs bor (f7code_of(Tag) bsl 24) bor (f3code_of(Tag) bsl 11) bor 2#0110011,
	{I, <<Code:32/little>>, Offset};
encode_instr({{auipc, {x, N}, Address} = I, Offset}) ->
	Code = (Address bsl 11) bor (N bsl 6) bor 2#0010111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{lui, {x, N}, Address} = I, Offset}) ->
	Code = (Address bsl 11) bor (N bsl 6) bor 2#0110111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{j, Address} = I, Offset}) ->
	Code = (Address bsl 11) bor 2#1101111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{jalr, {x, N1}, {x, N2}} = I, Offset}) ->
	Code = (N2 bsl 14) bor (N1 bsl 6) bor 2#1100111,
	{I, <<Code:32/little>>, Offset};
encode_instr({{mret} = I, Offset}) ->
	{I, <<16#30200073:32/little>>, Offset};
encode_instr({{wfi} = I, Offset}) ->
	{I, <<16#10500073:32/little>>, Offset};
encode_instr(Any) ->
	Any.

write_detail([{Instr, Encoded, Loc} | Rest], OffsetMap, IO_Dev) ->
	case maps:find(Loc, OffsetMap) of
		{ok, Labels} ->
			lists:foreach(fun(L) -> io:format(IO_Dev, "\t~s~n", [L]) end, Labels);
		_ ->
			ok
	end,
	io:format(IO_Dev, "~8.16.0B\t~s\t~w~n", [Loc, fmt_code(Encoded), Instr]),
	write_detail(Rest, OffsetMap, IO_Dev);
write_detail([], _, _) ->
	ok.

fmt_code(<<A, B, C, D>>) ->
	io_lib:format("~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B", [D, C, B, A]).

write_binary([{_, Raw, _} | Rest], IO_Dev) ->
	file:write(IO_Dev, Raw),
	write_binary(Rest, IO_Dev);
write_binary([], _) ->
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
f3code_of(sra)		-> 2#101.

f7code_of(sub)		-> 2#0100000;
f7code_of(sra)		-> 2#0100000;
f7code_of(_)		-> 2#0000000.

