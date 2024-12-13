-module(e_dumper_machine1).
-export([generate_code/4]).
-include("e_riscv.hrl").

-type scan_context() :: #{
                          label_map := #{atom() => non_neg_integer()},
                          offset_map := #{non_neg_integer() => [atom()]},
                          start_address := non_neg_integer(),
                          wordsize := non_neg_integer()
                         }.

-spec generate_code(flatten_irs(), flatten_irs(), string(), e_compile_option:option()) -> ok.
generate_code(CodeIRs, IVecIRs, OutputFile, #{wordsize := WordSize}) ->
    ScanContext = #{label_map => #{}, offset_map => #{}, start_address => 0, wordsize => WordSize},
    %% User Code
    {CodeWithPos, CodeCtx} = scan_address(CodeIRs, 0, [], ScanContext),
    #{label_map := LabelMap, offset_map := OffsetMap, start_address := CodeStartAddress} = CodeCtx,
    Code = [encode_instr(A) || A <- replace_address(CodeWithPos, LabelMap)],
    %% Interrupt Vector
    {IVecWithPos, IVecCtx} = scan_address(IVecIRs, 0, [], ScanContext),
    IVec = [encode_instr(A) || A <- replace_address(IVecWithPos, LabelMap)],
    #{start_address := IVecStartAddress} = IVecCtx,
    %% Write bin files
    e_util:file_write(OutputFile ++ ".code.bin",
                      fun(IO) -> write_binary(Code, CodeStartAddress, IO) end),
    e_util:file_write(OutputFile ++ ".ivec.bin",
                      fun(IO) -> write_binary(IVec, IVecStartAddress, IO) end),
    %% Write detail files
    e_util:file_write(OutputFile ++ ".code.detail.txt",
                      fun(IO) -> write_detail(Code, CodeStartAddress, OffsetMap, IO) end),
    e_util:file_write(OutputFile ++ ".ivec.detail.txt",
                      fun(IO) -> write_detail(IVec, IVecStartAddress, OffsetMap, IO) end),
    ok.

-spec scan_address([tuple()], non_neg_integer(), [tuple()], scan_context()) ->
          {[tuple()], scan_context()}.
scan_address([{start_address, N} | Rest], 0, Result, #{start_address := 0} = Ctx) ->
    scan_address(Rest, N, Result, Ctx#{start_address := N});
scan_address([{start_address, _} | _], N, _, _) when N > 0 ->
    throw({start_address_error, "the start_address command can not appear more than once"});
scan_address([{label, {align, N}, Name} | Rest], Offset, Result,
             #{label_map := LabelMap, offset_map := OffsetMap} = Ctx) ->
    check_label_conflict(Name, LabelMap),
    FixedOffset = e_util:fill_unit_pessi(Offset, N),
    NewLabelMap = LabelMap#{Name => FixedOffset},
    NewOffsetMap = maps:update_with(FixedOffset, fun(Ns) -> [Name | Ns] end, [Name], OffsetMap),
    scan_address(Rest, FixedOffset, Result,
                 Ctx#{label_map := NewLabelMap, offset_map := NewOffsetMap});
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

check_label_conflict(Name, LabelMap) ->
    case maps:find(Name, LabelMap) of
        {ok, _} ->
            e_util:ethrow({"", 0, 0}, "Name conflict on label \"~s\"", [Name]);
        _ ->
            ok
    end.

-define(IS_BR_INSTR(Tag),
        (
          Tag =:= beq orelse Tag =:= bne orelse Tag =:= bge orelse
          Tag =:= blt orelse Tag =:= bgt orelse Tag =:= ble
        )).

replace_address([{{Br, R1, R2, Label}, Loc} | Rest], LabelMap) when ?IS_BR_INSTR(Br) ->
    {ok, Address} = maps:find(Label, LabelMap),
    RelativeOffset = Address - Loc,
    if
        RelativeOffset > 4095 orelse RelativeOffset < -4096 ->
            throw({instruction_error, Br, {out_of_range, RelativeOffset}});
        true ->
            [{{Br, R1, R2, RelativeOffset}, Loc} | replace_address(Rest, LabelMap)]
    end;
replace_address([{{j, Label}, Loc} | Rest], LabelMap) ->
    {ok, Address} = maps:find(Label, LabelMap),
    RelativeOffset = Address - Loc,
    if
        RelativeOffset > 16#7FFFF orelse RelativeOffset < -16#80000 ->
            throw({instruction_error, j, {out_of_range, RelativeOffset}});
        true ->
            [{{j, RelativeOffset}, Loc} | replace_address(Rest, LabelMap)]
    end;
replace_address([{{la, R, Label}, Loc} | Rest], LabelMap) ->
    {ok, Address} = maps:find(Label, LabelMap),
    {High, Low} = e_util:u_type_immedi(Address - Loc),
    [{{auipc, R, High}, Loc}, {{addi, R, R, Low}, Loc + 4} | replace_address(Rest, LabelMap)];
replace_address([{{code, Label}, Loc} | Rest], LabelMap) when is_atom(Label) ->
    {ok, Address} = maps:find(Label, LabelMap),
    [{{code, Address}, Loc} | replace_address(Rest, LabelMap)];
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
          Tag =:= add orelse Tag =:= sub orelse Tag =:= 'and' orelse Tag =:= 'or' orelse
          Tag =:= 'xor' orelse Tag =:= sll orelse Tag =:= sra orelse
          Tag =:= mul orelse Tag =:= mulh orelse Tag =:= 'div' orelse Tag =:= 'rem'
        )).

encode_instr({{Br, {x, N1}, {x, N2}, Address} = I, Loc})
  when Br =:= bge; Br =:= blt; Br =:= beq; Br =:= bne ->
    {High, Low} = e_util:b_type_immedi(Address),
    Immedi = (High bsl 25) bor (Low bsl 7),
    Code =  Immedi bor (N2 bsl 20) bor (N1 bsl 15) bor (f3code_of(Br) bsl 12) bor 2#1100011,
    {I, <<Code:32/little>>, Loc};
encode_instr({{S, {x, N1}, {{x, N2}, O}} = I, Loc})
  when S =:= sw; S =:= sb ->
    {High, Low} = e_util:s_type_immedi(O),
    Immedi = (High bsl 25) bor (Low bsl 7),
    %% N2 is RS1, the pointer.
    Code = Immedi bor (N2 bsl 15) bor (N1 bsl 20) bor (f3code_of(S) bsl 12) bor 2#0100011,
    {I, <<Code:32/little>>, Loc};
encode_instr({{L, {x, N1}, {{x, N2}, O}} = I, Loc})
  when L =:= lw; L =:= lb ->
    Code = (O bsl 20) bor (N2 bsl 15) bor (f3code_of(L) bsl 12) bor (N1 bsl 7) bor 2#0000011,
    {I, <<Code:32/little>>, Loc};
%% SRAI is special, it's like `I` and `R` at the same time. both `f7` and immediate exists.
encode_instr({{srai, {x, N1}, {x, N2}, A} = I, Loc}) ->
    FixedA = A bor 2#010000000000,
    Code = (FixedA bsl 20) bor (N2 bsl 15) bor (f3code_of(srai) bsl 12) bor (N1 bsl 7) bor 2#0010011,
    {I, <<Code:32/little>>, Loc};
encode_instr({{Tag, {x, N1}, {x, N2}, A} = I, Loc})
  when ?IS_IMMEDI_CALC(Tag) ->
    Code = (A bsl 20) bor (N2 bsl 15) bor (f3code_of(Tag) bsl 12) bor (N1 bsl 7) bor 2#0010011,
    {I, <<Code:32/little>>, Loc};
encode_instr({{Tag, {x, N1}, {x, N2}, CSR} = I, Loc})
  when Tag =:= csrrw; Tag =:= csrrs; Tag =:= csrrc ->
    Code = (CSR bsl 20) bor (N2 bsl 15) bor (f3code_of(Tag) bsl 12) bor (N1 bsl 7) bor 2#1110011,
    {I, <<Code:32/little>>, Loc};
encode_instr({{Tag, {x, N1}, {x, N2}, {x, N3}} = I, Loc})
  when ?IS_NORMAL_CALC(Tag) ->
    Rs = (N3 bsl 20) bor (N2 bsl 15) bor (N1 bsl 7),
    Code = Rs bor (f7code_of(Tag) bsl 25) bor (f3code_of(Tag) bsl 12) bor 2#0110011,
    {I, <<Code:32/little>>, Loc};
encode_instr({{auipc, {x, N}, Address} = I, Loc}) ->
    Code = (Address bsl 12) bor (N bsl 7) bor 2#0010111,
    {I, <<Code:32/little>>, Loc};
encode_instr({{lui, {x, N}, Address} = I, Loc}) ->
    Code = (Address bsl 12) bor (N bsl 7) bor 2#0110111,
    {I, <<Code:32/little>>, Loc};
encode_instr({{j, Address} = I, Loc}) ->
    Code = (e_util:j_type_immedi(Address) bsl 12) bor 2#1101111,
    {I, <<Code:32/little>>, Loc};
encode_instr({{jalr, {x, N1}, {x, N2}} = I, Loc}) ->
    Code = (N2 bsl 15) bor (N1 bsl 7) bor 2#1100111,
    {I, <<Code:32/little>>, Loc};
encode_instr({{mret} = I, Loc}) ->
    {I, <<16#30200073:32/little>>, Loc};
encode_instr({{wfi} = I, Loc}) ->
    {I, <<16#10500073:32/little>>, Loc};
encode_instr({{string, _, _} = I, Loc}) ->
    {I, <<>>, Loc};
encode_instr({{code, N} = I, Loc}) when is_integer(N) ->
    {I, <<N:32/little>>, Loc};
encode_instr(Any) ->
    Any.

-define(NSPACE(N), lists:duplicate(N, $\s)).

write_detail([{{string, Content, Length}, _, Loc} | Rest], Loc, OffsetMap, IO) ->
    io:format(IO, "~8.16.0b:~s\"~s\\0\"~n", [Loc, ?NSPACE(31), e_util:fix_special_chars(Content)]),
    write_detail(Rest, Loc + Length + 1, OffsetMap, IO);
write_detail([{{code, _}, Raw, Loc} | Rest], Loc, OffsetMap, IO) ->
    io:format(IO, "~8.16.0b:~s~s~n", [Loc, ?NSPACE(7), fmt_code(Raw)]),
    write_detail(Rest, Loc + byte_size(Raw), OffsetMap, IO);
write_detail([{Instr, Raw, Loc} | Rest], Loc, OffsetMap, IO) ->
    case maps:find(Loc, OffsetMap) of
        {ok, Labels} ->
            [io:format(IO, "~s~s:~n", [?NSPACE(32), L]) || L <- lists:reverse(Labels)];
        _ ->
            ok
    end,
    io:format(IO, "~8.16.0b:~s~s~s~w~n", [Loc, ?NSPACE(7), fmt_code(Raw), ?NSPACE(16), Instr]),
    write_detail(Rest, Loc + byte_size(Raw), OffsetMap, IO);
write_detail([{_, _, Loc} | _] = Data, N, OffsetMap, IO) when Loc > N ->
    io:format(IO, "~8.16.0b:~s      00~n", [N, ?NSPACE(7)]),
    write_detail(Data, N + 1, OffsetMap, IO);
write_detail([], _, _, _) ->
    ok.

fmt_code(<<A, B, C, D>>) ->
    io_lib:format("~2.16.0b~2.16.0b~2.16.0b~2.16.0b", [D, C, B, A]);
fmt_code(<<>>) ->
    "".

write_binary([{{string, Content, Length}, _, Loc} | Rest], Loc, IO) ->
    file:write(IO, [Content, 0]),
    write_binary(Rest, Loc + Length + 1, IO);
write_binary([{_, Raw, Loc} | Rest], Loc, IO) ->
    file:write(IO, Raw),
    write_binary(Rest, Loc + byte_size(Raw), IO);
write_binary([{_, _, Loc} | _] = Data, N, IO) when Loc > N ->
    file:write(IO, [0]),
    write_binary(Data, N + 1, IO);
write_binary([], _, _) ->
    ok.

f3code_of(lw     ) -> 2#010;
f3code_of(sw     ) -> 2#010;
f3code_of(lb     ) -> 2#000;
f3code_of(sb     ) -> 2#000;
f3code_of(addi   ) -> 2#000;
f3code_of(beq    ) -> 2#000;
f3code_of(bne    ) -> 2#001;
f3code_of(blt    ) -> 2#100;
f3code_of(bge    ) -> 2#101;
f3code_of(andi   ) -> 2#111;
f3code_of(ori    ) -> 2#110;
f3code_of(xori   ) -> 2#100;
f3code_of(slli   ) -> 2#001;
f3code_of(srai   ) -> 2#101;
f3code_of(add    ) -> 2#000;
f3code_of(sub    ) -> 2#000;
f3code_of('and'  ) -> 2#111;
f3code_of('or'   ) -> 2#110;
f3code_of('xor'  ) -> 2#100;
f3code_of(sll    ) -> 2#001;
f3code_of(sra    ) -> 2#101;
f3code_of(mul    ) -> 2#000;
f3code_of(mulh   ) -> 2#001;
f3code_of('div'  ) -> 2#100;
f3code_of('rem'  ) -> 2#110;
f3code_of('csrrw') -> 2#001;
f3code_of('csrrs') -> 2#010;
f3code_of('csrrc') -> 2#011.

f7code_of(sub    ) -> 2#0100000;
f7code_of(sra    ) -> 2#0100000;
f7code_of(mul    ) -> 2#0000001;
f7code_of(mulh   ) -> 2#0000001;
f7code_of('div'  ) -> 2#0000001;
f7code_of('rem'  ) -> 2#0000001;
f7code_of(_      ) -> 2#0000000.
