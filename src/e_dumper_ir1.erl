-module(e_dumper_ir1).
-export([generate_code/3]).
-compile([{nowarn_unused_function, [{write_asm, 2}, {op_tag_str, 1}]}]).
-include("e_record_definition.hrl").
-include("e_riscv.hrl").

-type context() :: #{
                     tmp_regs := [machine_reg()],
                     free_regs := [machine_reg()], %% A subset of `tmp_regs`.
                     string_collector := pid(), %% A separate process to collect string literals.
                     cond_label := {atom(), atom()}, %% For logic operation (and, or, not) generating.
                     scope_tag := atom(),
                     epilogue_tag := atom(),
                     ret_offset := neg_integer(),
                     prefer_shift := boolean(),
                     wordsize := pos_integer()
                    }.

-spec generate_code(e_ast_compiler:ast_compile_result(), string(), e_compile_option:option()) -> ok.
generate_code({{InitCode, AST}, Vars}, OutputFile, Options) ->
    #{wordsize := WordSize, data_pos := DPos, data_size := DSize, code_pos := CPos, prefer_shift := PreferShift} = Options,
    #e_vars{shifted_size = ShiftedSize, size = GlobalVarSize} = Vars,
    %% Let's assume we need at least 128 bytes for stack.
    MinDataSize = GlobalVarSize + 128,
    e_util:assert(DSize >= MinDataSize, e_util:fmt("Data storage size (~w) is not enough. (~w needed)", [DSize, MinDataSize])),
    GP = DPos + DSize - ShiftedSize,
    {Init1, Init2} = init_code(DPos, GP, Options),
    Pid = spawn_link(fun() -> string_collect_loop([]) end),
    Regs = e_riscv_ir:tmp_regs(),
    Ctx = #{wordsize => WordSize, scope_tag => top, tmp_regs => Regs, free_regs => Regs, string_collector => Pid, epilogue_tag => none, ret_offset => -WordSize, prefer_shift => PreferShift, cond_label => {none, none}},
    InitVars0 = lists:map(fun(S) -> stmt_to_ir(S, Ctx#{scope_tag := '__init_vars'}) end, InitCode),
    InitVars = [{label, {align, 1}, '__init_vars'} | InitVars0],
    InterruptMap = collect_interrupt_map(AST, []),
    CodeIRs = ast_to_ir(AST, Ctx),
    StrList = string_collect_dump(Pid),
    StrIRs = lists:map(fun({L, S, Len}) -> [{label, {align, 1}, L}, {string, S, Len}] end, StrList),
    IRs = lists:flatten([{start_address, CPos}, Init1, InitVars, Init2, CodeIRs, StrIRs]),
    IVecIRs = ivec_irs([], 0, InterruptMap, Options),
    e_util:file_write(OutputFile ++ ".code.ir1", fun(IO) -> write_irs(IRs, IO) end),
    e_util:file_write(OutputFile ++ ".ivec.ir1", fun(IO) -> write_irs(IVecIRs, IO) end),
    e_util:file_write(OutputFile ++ ".code.asm", fun(IO) -> write_asm(IRs, IO) end),
    ok.

collect_interrupt_map([#e_function{name = Name, attribute = #{interrupt := N}, loc = Loc} | Rest], Result) ->
    case lists:keyfind(N, 1, Result) of
        {N, Prev} ->
            e_util:ethrow(Loc, "interrupt number ~w is taken by <~s>", [N, Prev]);
        _ ->
            collect_interrupt_map(Rest, [{N, Name} | Result])
    end;
collect_interrupt_map([_ | Rest], Result) ->
    collect_interrupt_map(Rest, Result);
collect_interrupt_map([], Result) ->
    maps:from_list(Result).

%% It's safe to use temporary register like {x, 5} in init code.
init_code(SP, GP, #{entry_function := Entry} = Options) ->
    %% It's safe to use x5 register in init code.
    EntryJump = [{label, {align, 1}, '__entry_jump'}, {la, {x, 5}, Entry}, {jalr, {x, 1}, {x, 5}}],
    EndJump = [{label, {align, 1}, '__end'}, {j, '__end'}],
    DefaultISR = [{label, {align, 1}, '__default_isr'}, {j, '__default_isr'}],
    %% Init `sp` and `gp`. This should be before InitVars.
    InitSystemIRs1 = [{label, {align, 1}, '__init_system1'}, e_riscv_ir:li({x, 2}, SP), e_riscv_ir:li({x, 3}, GP)],
    %% Set interrupt vector base address, enable interrupt.
    InitSystemIRs2 = [{label, {align, 1}, '__init_system2'}, interrupt_init_irs({x, 5}, Options)],
    Init1 = [{label, {align, 1}, '__init'}, InitSystemIRs1],
    Init2 = [InitSystemIRs2, EntryJump, EndJump, DefaultISR],
    {Init1, Init2}.

-spec interrupt_init_irs(machine_reg(), e_compile_option:option()) -> flatten_irs().
interrupt_init_irs(T, #{ivec_pos := Pos, ivec_size := Size}) when Size > 0 ->
    %% Initialize interrupt vector address by writting `mtvec`(CSR 0x305).
    %% CAUTION: In RV Spec, mtvec[1] is reserved. But QingKe need mtvec[1] to be 1 to be compatible with the standard.
    SetInterruptVector = [e_riscv_ir:li(T, Pos), {ori, T, T, 2#11}, {csrrw, {x, 0}, T, 16#305}],
    %% Initialize MIE and MPIE in `mstatus`(CSR 0x300).
    InitInterrupt = [e_riscv_ir:li(T, 16#88), {csrrw, {x, 0}, T, 16#300}],
    lists:flatten([InitInterrupt | SetInterruptVector]);
interrupt_init_irs(_, _) ->
    [].

%% On some platforms, the first 4-byte in interrupt vector is empty, and usually used for init jump.
ivec_irs([], 0, InterruptMap, #{ivec_pos := Pos, ivec_init_jump := true, wordsize := WordSize} = Options) ->
    ivec_irs([{j, '__init'}, {start_address, Pos}], WordSize, InterruptMap, Options);
ivec_irs([], 0, InterruptMap, #{ivec_pos := Pos} = Options) ->
    ivec_irs([{start_address, Pos}], 0, InterruptMap, Options);
ivec_irs(R, N, InterruptMap, #{ivec_size := Size, wordsize := WordSize} = Options) when N < Size ->
    ISR_Label = maps:get(N div WordSize, InterruptMap, '__default_isr'),
    ivec_irs([{code, ISR_Label} | R], N + WordSize, InterruptMap, Options);
ivec_irs(R, Size, _, #{ivec_size := Size}) ->
    lists:reverse(R).

-spec ast_to_ir(e_ast(), context()) -> irs().
ast_to_ir([#e_function{name = Name, stmts = Stmts, vars = #e_vars{size = Size0, shifted_size = Size1}} = Fn | Rest], Ctx) ->
    #{wordsize := WordSize, free_regs := [T | _] = Regs} = Ctx,
    %% When there are no local variables, there should still be one word for returning value.
    Size2 = max(e_util:fill_unit_pessi(Size1, WordSize), WordSize),
    %% The extra `2` words are for `frame pointer`({x, 8}) and `returning address`({x, 1}).
    FrameSize = Size2 + WordSize * 2,
    SaveFpRa = [{sw, {x, 8}, {{x, 2}, -WordSize * 2}}, {sw, {x, 1}, {{x, 2}, -WordSize}}],
    RestoreFpRa = [{lw, {x, 8}, {{x, 2}, -WordSize * 2}}, {lw, {x, 1}, {{x, 2}, -WordSize}}],
    RegSave = [e_riscv_ir:addi({x, 2}, FrameSize, T), SaveFpRa, e_riscv_ir:mv({x, 8}, {x, 2}), e_riscv_ir:addi({x, 8}, -FrameSize, T)],
    {I1, I2} = interrupt_related_code(Fn, Regs, Ctx),
    RegRestore = [RestoreFpRa, e_riscv_ir:addi({x, 2}, -FrameSize, T)],
    EpilogueTag = generate_tag(Name, epilogue),
    Prologue = [RegSave, I1, {comment, "prologue end"}],
    Epilogue = [{label, {align, 1}, EpilogueTag}, I2, RegRestore, ret_instruction_of(Fn)],
    Ctx1 = Ctx#{scope_tag := Name, epilogue_tag := EpilogueTag, ret_offset := Size1 - Size0 - WordSize},
    Body = lists:map(fun(S) -> stmt_to_ir(S, Ctx1) end, Stmts),
    %% The result should be flattened before calling `fix_irs/1`.
    FinalIRs = lists:flatten([{fn, Name}, Prologue, Body, Epilogue | ast_to_ir(Rest, Ctx)]),
    fix_irs(FinalIRs);
ast_to_ir([_ | Rest], Ctx) ->
    ast_to_ir(Rest, Ctx);
ast_to_ir([], _) ->
    [].

interrupt_related_code(#e_function{attribute = #{interrupt := _}}, Regs, Ctx) ->
    reg_save_restore(Regs, Ctx);
interrupt_related_code(_, _, _) ->
    {[], []}.

ret_instruction_of(#e_function{attribute = #{interrupt := _}}) ->
    [{mret}];
ret_instruction_of(_) ->
    [{jalr, {x, 0}, {x, 1}}].

-spec stmt_to_ir(e_stmt(), context()) -> irs().
stmt_to_ir(#e_if_stmt{'cond' = Cond, then = Then0, 'else' = Else0, loc = Loc}, #{scope_tag := ScopeTag} = Ctx) ->
    ThenLabel = generate_tag(ScopeTag, if_then, Loc),
    ElseLabel = generate_tag(ScopeTag, if_else, Loc),
    EndLabel = generate_tag(ScopeTag, if_end, Loc),
    {CondIRs, R_Bool, _} = expr_to_ir(Cond, Ctx#{cond_label => {ThenLabel, ElseLabel}}),
    %% The branch/jump must be following CondIRs since it relys on the result register of CondIRs.
    CondWithJmp = [CondIRs, 'br!_reg'(R_Bool, ElseLabel)],
    StartComment = comment('if', e_util:stmt_to_str(Cond), Loc),
    EndComment = comment('if', "end", Loc),
    Then1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Then0),
    Else1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Else0),
    Then2 = [{label, {align, 1}, ThenLabel}, Then1, {j, EndLabel}],
    Else2 = [{label, {align, 1}, ElseLabel}, Else1, {label, {align, 1}, EndLabel}],
    [StartComment, CondWithJmp, Then2, Else2, EndComment];
stmt_to_ir(#e_while_stmt{'cond' = Cond, stmts = Stmts0, loc = Loc}, #{scope_tag := ScopeTag} = Ctx) ->
    StartLabel = generate_tag(ScopeTag, while_start, Loc),
    BodyLabel = generate_tag(ScopeTag, while_body, Loc),
    EndLabel = generate_tag(ScopeTag, while_end, Loc),
    {CondIRs, R_Bool, _} = expr_to_ir(Cond, Ctx#{cond_label => {BodyLabel, EndLabel}}),
    StartComment = comment(while, e_util:stmt_to_str(Cond), Loc),
    EndComment = comment(while, "end", Loc),
    RawBody = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Stmts0),
    Body = [{label, {align, 1}, BodyLabel}, RawBody, {j, StartLabel}, {label, {align, 1}, EndLabel}],
    [StartComment, {label, {align, 1}, StartLabel}, CondIRs, 'br!_reg'(R_Bool, EndLabel), Body, EndComment];
stmt_to_ir(#e_return_stmt{expr = none}, #{epilogue_tag := EpilogueTag}) ->
    [{comment, "return"}, {j, EpilogueTag}];
stmt_to_ir(#e_return_stmt{expr = Expr}, #{epilogue_tag := EpilogueTag, ret_offset := Offset} = Ctx) ->
    {ExprIRs, R, _} = expr_to_ir(Expr, Ctx),
    %% We use stack to pass result
    [ExprIRs, {comment, "prepare return value"}, {sw, R, {{x, 8}, Offset}}, {j, EpilogueTag}];
stmt_to_ir(#e_goto_stmt{label = Label}, #{scope_tag := ScopeTag}) ->
    [{j, generate_tag(ScopeTag, Label)}];
stmt_to_ir(#e_label{name = Label}, #{scope_tag := ScopeTag}) ->
    [{label, {align, 1}, generate_tag(ScopeTag, Label)}];
stmt_to_ir(Stmt, Ctx) ->
    {Exprs, _, _} = expr_to_ir(Stmt, Ctx),
    Exprs.

generate_tag(ScopeTag, Tag, {_, Line, Column}) ->
    list_to_atom(e_util:fmt("~s_~s_~w_~w", [ScopeTag, Tag, Line, Column])).

generate_tag(ScopeTag, Tag) ->
    list_to_atom(e_util:fmt("~s_~s", [ScopeTag, Tag])).

comment(Tag, Info, {Filename, Line, Col}) ->
    [{comment, io_lib:format("[~s@~s:~w:~w] ~s", [Tag, Filename, Line, Col, Info])}].

-spec expr_to_ir(e_expr(), context()) -> {irs(), machine_reg(), context()}.
expr_to_ir(?OP2('=', ?OP2('^', ?OP2('+', Expr, ?I(N)), ?I(V)), Right), Ctx) when ?IS_IMM12(N) ->
    {RightIRs, R1, Ctx1} = expr_to_ir(Right, Ctx),
    {VarrefIRs, R2, Ctx2} = expr_to_ir(Expr, Ctx1),
    {[RightIRs, VarrefIRs, {st_tag(V), R1, {R2, N}}], R1, Ctx2};
expr_to_ir(?OP2('=', ?OP2('^', Expr, ?I(V)), Right), Ctx) ->
    {RightIRs, R1, Ctx1} = expr_to_ir(Right, Ctx),
    {LeftIRs, R2, Ctx2} = expr_to_ir(Expr, Ctx1),
    {[RightIRs, LeftIRs, {st_tag(V), R1, {R2, 0}}], R1, Ctx2};
expr_to_ir(?OP2('^', ?OP2('+', Expr, ?I(N)), ?I(V)), Ctx) when ?IS_IMM12(N) ->
    {IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
    {[IRs, {ld_tag(V), T, {R, N}}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?OP2('^', Expr, ?I(V)), Ctx) ->
    {IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
    {[IRs, {ld_tag(V), T, {R, 0}}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?CALL(Fn, Args), Ctx) ->
    %% register preparing and restoring steps
    #{free_regs := FreeRegs, tmp_regs := TmpRegs, wordsize := WordSize} = Ctx,
    {BeforeCall, AfterCall} = reg_save_restore(TmpRegs -- FreeRegs, Ctx),
    %% The calling related steps
    {FnLoad, R, Ctx1} = expr_to_ir(Fn, Ctx),
    PreserveRet = [{comment, "preserve space for ret"}, {addi, {x, 2}, {x, 2}, WordSize}],
    {ArgPrepare, N} = args_to_stack(Args, 0, [], Ctx1),
    #{free_regs := [T | _]} = Ctx1,
    StackRestore = [{comment, "drop args"}, e_riscv_ir:addi({x, 2}, -N, T)],
    RetLoad = [{comment, "load ret"}, {lw, R, {{x, 2}, -WordSize}}, {addi, {x, 2}, {x, 2}, -WordSize}],
    Call = [FnLoad, PreserveRet, {comment, "args"}, ArgPrepare, {comment, "call"}, {jalr, {x, 1}, R}, StackRestore, RetLoad],
    {[{comment, "call start"}, BeforeCall, Call, AfterCall, {comment, "call end"}], R, Ctx1};
%% Optimization for `* 1` is important since it speed up the accessing of byte arrays.
expr_to_ir(?OP2('*', Expr, ?I(1)), Ctx) ->
    expr_to_ir(Expr, Ctx);
%% Translate `*` to `bsl` and `+` when option `prefer_shift` is given.
expr_to_ir(?OP2('*', Expr, ?I(N)), #{prefer_shift := true} = Ctx) when N > 1 ->
    {IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
    Nums = lists:map(fun(A) -> trunc(math:log2(A)) end, e_util:dissociate_num(N, 1 bsl 32)),
    ShiftIRs = assemble_shifts(Nums, R, T, Ctx#{free_regs := RestRegs}),
    {[IRs, e_riscv_ir:mv(T, {x, 0}), ShiftIRs], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
%% RISC-V do not have immediate version `sub` instruction, convert `-` to `+` to make use of `addi` later.
expr_to_ir(?OP2('-', Expr, ?I(N)) = OP, Ctx) ->
    expr_to_ir(OP?OP2('+', Expr, ?I(-N)), Ctx);
%% More immediate related optimizations
expr_to_ir(?OP2(Tag, Expr, ?I(0)), Ctx) when Tag =:= '+'; Tag =:= 'bor'; Tag =:= 'bxor'; ?IS_SHIFT(Tag) ->
    expr_to_ir(Expr, Ctx);
expr_to_ir(?OP2('band', Expr, ?I(-1)), Ctx) ->
    expr_to_ir(Expr, Ctx);
%% The immediate ranges for shifting instructions are different from other immediate ranges.
expr_to_ir(?OP2(Tag, _, ?I(N), Loc), _) when ?IS_SHIFT(Tag), N < 0 ->
    e_util:ethrow(Loc, "negative shift number.");
expr_to_ir(?OP2(Tag, _, ?I(N), Loc), #{wordsize := WordSize}) when ?IS_SHIFT(Tag), N > WordSize * 8 ->
    e_util:ethrow(Loc, "shift number `~w` is out of range.", [N]);
expr_to_ir(?OP2(Tag, Expr, ?I(N)), Ctx) when ?IS_SHIFT(Tag) ->
    {IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
    {[IRs, {e_riscv_ir:to_op_immedi(Tag), T, R, N}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?OP2(Tag, Expr, ?I(N)), Ctx) when ?IS_IMMID_ARITH(Tag), ?IS_IMM12(N) ->
    {IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
    {[IRs, {e_riscv_ir:to_op_immedi(Tag), T, R, N}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_ARITH(Tag) ->
    op3_to_ir(e_riscv_ir:to_op_normal(Tag), Left, Right, Ctx);
%% The tags for comparing operations are not translated here, it will be merged with the pseudo `br` or `br!`.
expr_to_ir(?OP2('<=', Left, Right) = Op, Ctx) ->
    expr_to_ir(Op?OP2('>=', Right, Left), Ctx);
expr_to_ir(?OP2('>', Left, Right) = Op, Ctx) ->
    expr_to_ir(Op?OP2('<', Right, Left), Ctx);
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_COMPARE(Tag) ->
    op3_to_ir(Tag, Left, Right, Ctx);
%% `and` and `or` do not consume tmp registers, it returns the same context and {x, 0} as a sign.
expr_to_ir(?OP2('and', Left, Right, Loc), #{scope_tag := ScopeTag, cond_label := {L1, L2}} = Ctx) ->
    L_Middle = generate_tag(ScopeTag, and_middle, Loc),
    {IRs1, R1, _} = expr_to_ir(Left, Ctx#{cond_label := {L_Middle, L2}}),
    {IRs2, R2, _} = expr_to_ir(Right, Ctx#{cond_label := {L1, L2}}),
    {[IRs1, 'br!_reg'(R1, L2), {label, {align, 1}, L_Middle}, IRs2, 'br!_reg'(R2, L2), {j, L1}], {x, 0}, Ctx};
expr_to_ir(?OP2('or', Left, Right, Loc), #{scope_tag := ScopeTag, cond_label := {L1, L2}} = Ctx) ->
    L_Middle = generate_tag(ScopeTag, or_middle, Loc),
    {IRs1, R1, _} = expr_to_ir(Left, Ctx#{cond_label := {L1, L_Middle}}),
    {IRs2, R2, _} = expr_to_ir(Right, Ctx#{cond_label := {L1, L2}}),
    {[IRs1, br_reg(R1, L1), {label, {align, 1}, L_Middle}, IRs2, br_reg(R2, L1), {j, L2}], {x, 0}, Ctx};
expr_to_ir(?OP1('not', Expr), #{cond_label := {L1, L2}} = Ctx) ->
    {IRs, R, Ctx1} = expr_to_ir(Expr, Ctx#{cond_label := {L2, L1}}),
    {[IRs, 'br!_reg'(R, L1), {j, L2}], {x, 0}, Ctx1};
%% RISC-V do not have instruction for `bnot`, use `xor` to do that.
expr_to_ir(?OP1('bnot', Expr), Ctx) ->
    {IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
    {[IRs, {xori, T, R, -1}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?OP1('-', ?I(N) = Num), Ctx) when ?IS_IMM12(-N) ->
    expr_to_ir(Num?I(-N), Ctx);
expr_to_ir(?OP1('-', Expr), Ctx) ->
    {IRs, R, Ctx1} = expr_to_ir(Expr, Ctx),
    {[IRs, {sub, R, {x, 0}, R}], R, Ctx1};
expr_to_ir(?S(String, Loc), #{free_regs := [R | RestRegs], string_collector := Pid} = Ctx) ->
    Label = generate_tag(g, s, Loc),
    Pid ! {put, Label, String, length(String)},
    {[{la, R, Label}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(?I(0), Ctx) ->
    {[], {x, 0}, Ctx};
expr_to_ir(?I(N), #{free_regs := [R | RestRegs]} = Ctx) ->
    {e_riscv_ir:li(R, N), R, Ctx#{free_regs := RestRegs}};
expr_to_ir(?VREF(fp), Ctx) ->
    {[], {x, 8}, Ctx};
expr_to_ir(?VREF(gp), Ctx) ->
    {[], {x, 3}, Ctx};
expr_to_ir(?VREF(Name), #{free_regs := [R | RestRegs]} = Ctx) ->
    {[{la, R, Name}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(Any, _) ->
    e_util:ethrow(element(2, Any), "IR1: unsupported expr \"~w\"", [Any]).

-spec op3_to_ir(atom(), e_expr(), e_expr(), context()) -> {irs(), machine_reg(), context()}.
op3_to_ir(Tag, Left, Right, Ctx) ->
    {IRs1, R1, Ctx1} = expr_to_ir(Left, Ctx),
    {IRs2, R2, #{free_regs := [T | RestRegs]}} = expr_to_ir(Right, Ctx1),
    {[IRs1, IRs2, {Tag, T, R1, R2}], T, Ctx#{free_regs := recycle_tmpreg([R2, R1], RestRegs)}}.

%% The result is stored back to T1.
-spec assemble_shifts([non_neg_integer()], machine_reg(), machine_reg(), context()) -> irs().
assemble_shifts([0 | RestNums], R, T1, Ctx) ->
    [{add, T1, T1, R} | assemble_shifts(RestNums, R, T1, Ctx)];
assemble_shifts([N | RestNums], R, T1, #{free_regs := [T2 | _]} = Ctx) ->
    [e_riscv_ir:mv(T2, R), {slli, T2, T2, N}, {add, T1, T1, T2} | assemble_shifts(RestNums, R, T1, Ctx)];
assemble_shifts([], _, _, _) ->
    [].

-spec fix_irs(irs()) -> irs().
fix_irs([{Tag, Rd, R1, R2}, {'br!', Rd, DestTag} | Rest]) ->
    fix_irs([{reverse_cmp_tag(Tag), Rd, R1, R2}, {br, Rd, DestTag} | Rest]);
fix_irs([{Tag, Rd, R1, R2}, {br, Rd, DestTag} | Rest]) ->
    [{e_riscv_ir:to_cmp_op(Tag), R1, R2, DestTag} | fix_irs(Rest)];
%% Continuous and duplicated jump instructions (to the same address) without any labels in between are useless.
fix_irs([{j, Label} = I, {j, Label} | Rest]) ->
    fix_irs([I | Rest]);
fix_irs([{j, Label} = I | Rest]) ->
    case has_label_before_op(Label, Rest) of
        true ->
            fix_irs(Rest);
        false ->
            [I | fix_irs(Rest)]
    end;
fix_irs([{addi, R1, R2, N1}, {addi, R1, R1, N2} | Rest]) when ?IS_IMM12(N1 + N2) ->
    fix_irs([{addi, R1, R2, N1 + N2} | Rest]);
fix_irs([Any | Rest]) ->
    [Any | fix_irs(Rest)];
fix_irs([]) ->
    [].

has_label_before_op(Name, [{label, _, Name} | _]) ->
    true;
has_label_before_op(Name, [{label, _, _} | Rest]) ->
    has_label_before_op(Name, Rest);
has_label_before_op(_, _) ->
    false.

string_collect_loop(Collected) ->
    receive
        {put, Tag, String, Length} ->
            string_collect_loop([{Tag, String, Length} | Collected]);
        {dump, Pid} ->
            Pid ! {self(), Collected}
    end.

string_collect_dump(Pid) ->
    Pid ! {dump, self()},
    receive
        {Pid, Collected} ->
            Collected
    end.

-spec recycle_tmpreg([machine_reg()], [machine_reg()]) -> [machine_reg()].
recycle_tmpreg([R | Regs], RegBank) when ?IS_SPECIAL_REG(R) ->
    recycle_tmpreg(Regs, RegBank);
recycle_tmpreg([R | Regs], RegBank) ->
    recycle_tmpreg(Regs, [R | RegBank]);
recycle_tmpreg([], RegBank) ->
    RegBank.

%% There are limited number of regs, which means immediate number is small enough for single `lw`/`sw`.
-spec reg_save_restore([machine_reg()], context()) -> {irs(), irs()}.
reg_save_restore(Regs, #{wordsize := WordSize, free_regs := [T | _]}) ->
    TotalSize = length(Regs) * WordSize,
    StackGrow = [{comment, "grow stack"}, e_riscv_ir:addi({x, 2}, TotalSize, T)],
    StackShrink = [{comment, "shrink stack"}, e_riscv_ir:addi({x, 2}, -TotalSize, T)],
    Save = e_util:list_map(fun(R, I) -> {sw, R, {{x, 2}, I * WordSize - TotalSize}} end, Regs),
    Restore = e_util:list_map(fun(R, I) -> {lw, R, {{x, 2}, I * WordSize - TotalSize}} end, Regs),
    Enter = [StackGrow, {comment, io_lib:format("regs to save: ~w", [Regs])}, Save],
    Leave = [{comment, io_lib:format("regs to restore: ~w", [Regs])}, Restore, StackShrink],
    {Enter, Leave}.

-spec args_to_stack(irs(), non_neg_integer(), irs(), context()) -> {irs(), non_neg_integer()}.
args_to_stack([Arg | Rest], N, Result, #{wordsize := WordSize} = Ctx) ->
    {IRs, R, _} = expr_to_ir(Arg, Ctx),
    PushIRs = [IRs, {addi, {x, 2}, {x, 2}, WordSize}, {sw, R, {{x, 2}, -WordSize}}],
    args_to_stack(Rest, N + WordSize, [PushIRs | Result], Ctx);
args_to_stack([], N, Result, _) ->
    {lists:reverse(Result), N}.

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], IO) when is_list(IRs) ->
    write_irs(IRs, IO),
    write_irs(Rest, IO);
write_irs([{comment, Content} | Rest], IO) ->
    io:format(IO, "    %% ~s~n", [Content]),
    write_irs(Rest, IO);
write_irs([{Tag, _} = IR | Rest], IO) when Tag =:= fn; Tag =:= label ->
    io:format(IO, "~w.~n", [IR]),
    write_irs(Rest, IO);
write_irs([IR | Rest], IO) ->
    io:format(IO, "    ~w.~n", [IR]),
    write_irs(Rest, IO);
write_irs([], _) ->
    ok.

-spec write_asm(irs(), file:io_device()) -> ok.
write_asm([IRs | Rest], IO) when is_list(IRs) ->
    write_asm(IRs, IO),
    write_asm(Rest, IO);
%% Normal 3-operand instructions
write_asm([{Tag, Op1, Op2, Op3} | Rest], IO) when Tag =:= csrrw; Tag =:= csrrs; Tag =:= csrrc ->
    io:format(IO, "    ~s ~s, ~s, ~s~n", [Tag | lists:map(fun op_tag_str/1, [Op1, Op3, Op2])]),
    write_asm(Rest, IO);
write_asm([{Tag, Op1, Op2, Op3} | Rest], IO) ->
    io:format(IO, "    ~s ~s, ~s, ~s~n", [Tag | lists:map(fun op_tag_str/1, [Op1, Op2, Op3])]),
    write_asm(Rest, IO);
%% Load and Store instructions
write_asm([{Tag, Op1, {{x, _} = Op2, N}} | Rest], IO) ->
    io:format(IO, "    ~s ~s, ~w(~s)~n", [Tag, op_tag_str(Op1), N, op_tag_str(Op2)]),
    write_asm(Rest, IO);
%% Labels
write_asm([{Tag, Name} | Rest], IO) when Tag =:= fn; Tag =:= label ->
    io:format(IO, "~s:~n", [Name]),
    write_asm(Rest, IO);
write_asm([{label, {align, 1}, Name} | Rest], IO) ->
    io:format(IO, "~s:~n", [Name]),
    write_asm(Rest, IO);
write_asm([{label, {align, N}, Name} | Rest], IO) ->
    io:format(IO, "    .balign ~w~n~s:~n", [N, Name]),
    write_asm(Rest, IO);
write_asm([{string, Content, _} | Rest], IO) ->
    io:format(IO, "    .string \"~ts\"~n", [e_util:fix_special_chars(Content)]),
    write_asm(Rest, IO);
write_asm([{start_address, Address} | Rest], IO) ->
    io:format(IO, "    .org ~w~n", [Address]),
    write_asm(Rest, IO);
write_asm([{code, Label} | Rest], IO) when is_atom(Label) ->
    io:format(IO, "    .word ~s~n", [Label]),
    write_asm(Rest, IO);
write_asm([{code, Number} | Rest], IO) when is_integer(Number) ->
    io:format(IO, "    .word ~w~n", [Number]),
    write_asm(Rest, IO);
write_asm([{comment, Content} | Rest], IO) ->
    io:format(IO, "    ## ~s~n", [Content]),
    write_asm(Rest, IO);
write_asm([{Tag, Op1, Op2} | Rest], IO) ->
    io:format(IO, "    ~s ~s, ~s~n", [Tag, op_tag_str(Op1), op_tag_str(Op2)]),
    write_asm(Rest, IO);
write_asm([{Tag, Op1} | Rest], IO) ->
    io:format(IO, "    ~s ~s~n", [Tag, op_tag_str(Op1)]),
    write_asm(Rest, IO);
write_asm([{Tag} | Rest], IO) ->
    io:format(IO, "    ~s~n", [Tag]),
    write_asm(Rest, IO);
write_asm([], _) ->
    ok.

-spec op_tag_str(integer() | machine_reg() | atom()) -> string().
op_tag_str(N) when is_integer(N) ->
    integer_to_list(N);
op_tag_str({x, N}) ->
    "x" ++ integer_to_list(N);
op_tag_str(Label) when is_atom(Label) ->
    atom_to_list(Label).

%% {x, 0} means that there is not branching to generate. (already generated in previous IRs)
'br!_reg'({x, 0}, _    ) -> [];
'br!_reg'(R     , Label) -> [{'br!', R, Label}].

br_reg   ({x, 0}, _    ) -> [];
br_reg   (R     , Label) -> [{'br', R, Label}].

reverse_cmp_tag('==') -> '!=';
reverse_cmp_tag('!=') -> '==';
reverse_cmp_tag('>=') -> '<' ;
reverse_cmp_tag('<=') -> '>' ;
reverse_cmp_tag('>' ) -> '<=';
reverse_cmp_tag('<' ) -> '>='.

%% Unsigned data types are dropped by E language, so instructions like `lbu` are not used.
st_tag(1) -> sb;
st_tag(_) -> sw.
ld_tag(1) -> lb;
ld_tag(_) -> lw.
