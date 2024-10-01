-module(e_dumper_ir1).
-export([generate_code/6]).
-include("e_record_definition.hrl").

%% According the calling convention of RISC-V: RA is X1, SP is X2, GP is X3, FP is X8. X5-X7 is T0-T2.
%% We use A0-A4 as T3-T7 since we pass arguments and result through stack and `Ax` are caller saved just like `Tx`.
-type machine_reg() :: {x, non_neg_integer()}.

-type context() ::
	#{
	%% `free_regs` will be a subset of `tmp_regs`. (And initialized as `tmp_regs`)
	tmp_regs		=> [machine_reg()],
	free_regs		=> [machine_reg()],
	%% The string literals are collected by a separate process to avoid complex functions.
	string_collector	=> pid(),
	%% `cond_label` is for generating logic operator (and, or, not) related code.
	cond_label		=> {atom(), atom()},
	scope_tag		=> atom(),
	epilogue_tag		=> atom(),
	ret_offset		=> non_neg_integer(),
	wordsize		=> pos_integer()
	}.

-define(IS_SPECIAL_REG(Tag),
	(
	Tag =:= {x, 8} orelse Tag =:= {x, 3} orelse Tag =:= {x, 2} orelse Tag =:= {x, 1} orelse Tag =:= {x, 0}
	)).

-define(IS_SMALL_IMMEDI(N),
	(
	N >= -2048 andalso N < 2048
	)).

-spec generate_code(e_ast(), e_ast(), non_neg_integer(), non_neg_integer(), string(), e_compile_option:option()) -> ok.
generate_code(AST, InitCode, SP, GP, OutputFile, #{wordsize := WordSize, entry_function := Entry, isr_vector_pos := InterruptVec}) ->
	Pid = spawn_link(fun() -> string_collect_loop([]) end),
	Regs = tmp_regs(),
	Ctx = #{wordsize => WordSize, scope_tag => top, tmp_regs => Regs, free_regs => Regs, string_collector => Pid, epilogue_tag => none, ret_offset => -WordSize},
	InitRegs = [smart_li({x, 2}, SP), smart_li({x, 3}, GP)],
	InitVars = [lists:map(fun(S) -> stmt_to_ir(S, Ctx#{scope_tag := '__init'}) end, InitCode)],
	[T | _] = Regs,
	%% `mtvec` is defined to be 0x305 in `priv-isa`.
	InitInterrupt = [smart_li(T, InterruptVec), {ori, T, T, 3}, {csrrw, {x, 0}, T, 16#305}],
	InitJump = stmt_to_ir(?CALL(#e_varref{name = Entry}, []), Ctx#{scope_tag := '__init'}),
	EndJump = [{label, {align, 1}, '__end'}, {j, '__end'}],
	DefaultISR = [{label, {align, 1}, '__default_isr'}, {j, '__default_isr'}],
	InitIRs = [{label, {align, 1}, '__init'}, InitRegs, InitVars, InitInterrupt, InitJump, EndJump, DefaultISR],
	IRs = ast_to_ir(AST, Ctx),
	StrTable = lists:map(fun({L, S, Len}) -> [{label, {align, 1}, L}, {string, S, Len}] end, string_collect_dump(Pid)),
	Fn1 = fun(IO_Dev) -> write_irs([{comment, "vim:ft=erlang"}, InitIRs, IRs, StrTable], IO_Dev) end,
	e_util:file_write(OutputFile, Fn1),
	Fn2 = fun(IO_Dev) -> write_asm([{comment, "For GNU assembler"}, InitIRs, IRs, StrTable], IO_Dev) end,
	e_util:file_write(OutputFile ++ ".asm", Fn2),
	ok.

-type irs() :: [tuple() | irs()].

-spec ast_to_ir(e_ast(), context()) -> irs().
ast_to_ir([#e_function{name = Name, stmts = Stmts, vars = #e_vars{size = Size0, shifted_size = Size1}} = Fn | Rest], Ctx) ->
	#{wordsize := WordSize, free_regs := Regs} = Ctx,
	%% When there are no local variables, there should still be one word for returning value.
	Size2 = erlang:max(e_util:fill_unit_pessi(Size1, WordSize), WordSize),
	%% The extra `2` words are for `frame pointer`({x, 8}) and `returning address`({x, 1}).
	FrameSize = Size2 + WordSize * 2,
	SaveFpRa = [{sw, {x, 8}, {{x, 2}, -WordSize * 2}}, {sw, {x, 1}, {{x, 2}, -WordSize}}],
	RestoreFpRa = [{lw, {x, 8}, {{x, 2}, -WordSize * 2}}, {lw, {x, 1}, {{x, 2}, -WordSize}}],
	RegSave = [smart_addi({x, 2}, FrameSize, Ctx), SaveFpRa, mv({x, 8}, {x, 2}), smart_addi({x, 8}, -FrameSize, Ctx)],
	{I1, I2} = interrupt_related_code(Fn, Regs, Ctx),
	RegRestore = [RestoreFpRa, smart_addi({x, 2}, -FrameSize, Ctx)],
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

interrupt_related_code(#e_function{interrupt = none}, _, _) ->
	{[], []};
interrupt_related_code(_, Regs, Ctx) ->
	reg_save_restore(Regs, Ctx).

ret_instruction_of(#e_function{interrupt = none}) ->
	{jalr, {x, 0}, {x, 1}};
ret_instruction_of(_) ->
	{mret}.

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

generate_tag(ScopeTag, Tag, {Line, Column}) ->
	list_to_atom(e_util:fmt("~s_~s_~w_~w", [ScopeTag, Tag, Line, Column])).

generate_tag(ScopeTag, Tag) ->
	list_to_atom(e_util:fmt("~s_~s", [ScopeTag, Tag])).

comment(Tag, Info, {Line, Col}) ->
	{comment, io_lib:format("[~s@~w:~w] ~s", [Tag, Line, Col, Info])}.

-spec expr_to_ir(e_expr(), context()) -> {irs(), machine_reg(), context()}.
expr_to_ir(?OP2('=', ?OP2('^', ?OP2('+', #e_varref{} = Var, ?I(N)), ?I(V)), Right), Ctx) when ?IS_SMALL_IMMEDI(N) ->
	{RightIRs, R1, Ctx1} = expr_to_ir(Right, Ctx),
	{VarrefIRs, R2, Ctx2} = expr_to_ir(Var, Ctx1),
	{[RightIRs, VarrefIRs, {st_tag(V), R1, {R2, N}}], R1, Ctx2};
expr_to_ir(?OP2('=', ?OP2('^', Expr, ?I(V)), Right), Ctx) ->
	{RightIRs, R1, Ctx1} = expr_to_ir(Right, Ctx),
	{LeftIRs, R2, Ctx2} = expr_to_ir(Expr, Ctx1),
	{[RightIRs, LeftIRs, {st_tag(V), R1, {R2, 0}}], R1, Ctx2};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{} = Var, ?I(N)), ?I(V)), Ctx) when ?IS_SMALL_IMMEDI(N) ->
	{IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Var, Ctx),
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
	StackRestore = [{comment, "drop args"}, smart_addi({x, 2}, -N, Ctx1)],
	RetLoad = [{comment, "load ret"}, {lw, R, {{x, 2}, -WordSize}}, {addi, {x, 2}, {x, 2}, -WordSize}],
	Call = [FnLoad, PreserveRet, {comment, "args"}, ArgPrepare, {comment, "call"}, {jalr, {x, 1}, R}, StackRestore, RetLoad],
	{[{comment, "call start"}, BeforeCall, Call, AfterCall, {comment, "call end"}], R, Ctx1};
%% RISC-V do not have immediate version `sub` instruction, convert `-` to `+` to make use of `addi` later.
expr_to_ir(?OP2('-', Expr, ?I(N)) = OP, Ctx) ->
	expr_to_ir(OP?OP2('+', Expr, ?I(-N)), Ctx);
expr_to_ir(?OP2(Tag, Expr, ?I(0)), Ctx) when Tag =:= '+'; Tag =:= 'bor'; Tag =:= 'bxor'; Tag =:= 'bsl'; Tag =:= 'bsr' ->
	expr_to_ir(Expr, Ctx);
expr_to_ir(?OP2('band', Expr, ?I(-1)), Ctx) ->
	expr_to_ir(Expr, Ctx);
%% The immediate ranges for shifting instructions are different from other immediate ranges.
expr_to_ir(?OP2(Tag, _, ?I(N), Loc), _) when (Tag =:= 'bsl' orelse Tag =:= 'bsr'), N < 0 ->
	e_util:ethrow(Loc, "shift number can not be negative but `~w` was given.", [N]);
expr_to_ir(?OP2(Tag, _, ?I(N), Loc), #{wordsize := 4}) when (Tag =:= 'bsl' orelse Tag =:= 'bsr'), N > 32 ->
	e_util:ethrow(Loc, "shift number `~w` is out of range.", [N]);
expr_to_ir(?OP2(Tag, _, ?I(N), Loc), #{wordsize := 8}) when (Tag =:= 'bsl' orelse Tag =:= 'bsr'), N > 64 ->
	e_util:ethrow(Loc, "shift number `~w` is out of range.", [N]);
expr_to_ir(?OP2(Tag, Expr, ?I(N)), Ctx) when Tag =:= 'bsl' orelse Tag =:= 'bsr' ->
	{IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
	{[IRs, {to_op_immedi(Tag), T, R, N}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?OP2(Tag, Expr, ?I(N)), Ctx) when ?IS_IMMID_ARITH(Tag), ?IS_SMALL_IMMEDI(N) ->
	{IRs, R, #{free_regs := [T | RestRegs]}} = expr_to_ir(Expr, Ctx),
	{[IRs, {to_op_immedi(Tag), T, R, N}], T, Ctx#{free_regs := recycle_tmpreg([R], RestRegs)}};
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_ARITH(Tag) ->
	op3_to_ir(to_op_normal(Tag), Left, Right, Ctx);
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
expr_to_ir(?OP1('-', ?I(N) = Num), Ctx) when ?IS_SMALL_IMMEDI(-N) ->
	expr_to_ir(Num?I(-N), Ctx);
expr_to_ir(?OP1('-', Expr), Ctx) ->
	{IRs, R, Ctx1} = expr_to_ir(Expr, Ctx),
	{[IRs, {sub, R, {x, 0}, R}], R, Ctx1};
expr_to_ir(#e_string{value = String, loc = Loc}, #{free_regs := [R | RestRegs], string_collector := Pid} = Ctx) ->
	Label = generate_tag(g, s, Loc),
	Pid ! {put, Label, String, length(String)},
	{[{la, R, Label}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(?I(0), Ctx) ->
	{[], {x, 0}, Ctx};
expr_to_ir(?I(N), #{free_regs := [R | RestRegs]} = Ctx) ->
	{smart_li(R, N), R, Ctx#{free_regs := RestRegs}};
expr_to_ir(#e_varref{name = fp}, Ctx) ->
	{[], {x, 8}, Ctx};
expr_to_ir(#e_varref{name = gp}, Ctx) ->
	{[], {x, 3}, Ctx};
expr_to_ir(#e_varref{name = Name}, #{free_regs := [R | RestRegs]} = Ctx) ->
	{[{la, R, Name}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(Any, _) ->
	e_util:ethrow(element(2, Any), "IR1: unsupported expr \"~w\"", [Any]).

op3_to_ir(Tag, Left, Right, Ctx) ->
	{IRs1, R1, Ctx1} = expr_to_ir(Left, Ctx),
	{IRs2, R2, #{free_regs := [T | RestRegs]}} = expr_to_ir(Right, Ctx1),
	{[IRs1, IRs2, {Tag, T, R1, R2}], T, Ctx#{free_regs := recycle_tmpreg([R2, R1], RestRegs)}}.

fix_irs([{Tag, Rd, R1, R2}, {'br!', Rd, DestTag} | Rest]) ->
	fix_irs([{reverse_cmp_tag(Tag), Rd, R1, R2}, {br, Rd, DestTag} | Rest]);
fix_irs([{Tag, Rd, R1, R2}, {br, Rd, DestTag} | Rest]) ->
	[{to_cmp_op(Tag), R1, R2, DestTag} | fix_irs(Rest)];
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
fix_irs([{addi, R1, R2, N1}, {addi, R1, R1, N2} | Rest]) when ?IS_SMALL_IMMEDI(N1 + N2) ->
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

recycle_tmpreg([R | Regs], RegBank) when ?IS_SPECIAL_REG(R) ->
	recycle_tmpreg(Regs, RegBank);
recycle_tmpreg([R | Regs], RegBank) ->
	recycle_tmpreg(Regs, [R | RegBank]);
recycle_tmpreg([], RegBank) ->
	RegBank.

%% There are limited number of regs, which means immediate number is small enough for single `lw`/`sw`.
reg_save_restore(Regs, #{wordsize := WordSize} = Ctx) ->
	TotalSize = length(Regs) * WordSize,
	StackGrow = [{comment, "grow stack"}, smart_addi({x, 2}, TotalSize, Ctx)],
	StackShrink = [{comment, "shrink stack"}, smart_addi({x, 2}, -TotalSize, Ctx)],
	Save = e_util:list_map(fun(R, I) -> {sw, R, {{x, 2}, I * WordSize - TotalSize}} end, Regs),
	Restore = e_util:list_map(fun(R, I) -> {lw, R, {{x, 2}, I * WordSize - TotalSize}} end, Regs),
	Enter = [StackGrow, {comment, io_lib:format("regs to save: ~w", [Regs])}, Save],
	Leave = [{comment, io_lib:format("regs to restore: ~w", [Regs])}, Restore, StackShrink],
	{Enter, Leave}.

args_to_stack([Arg | Rest], N, Result, #{wordsize := WordSize} = Ctx) ->
	{IRs, R, _} = expr_to_ir(Arg, Ctx),
	PushIRs = [IRs, {addi, {x, 2}, {x, 2}, WordSize}, {sw, R, {{x, 2}, -WordSize}}],
	args_to_stack(Rest, N + WordSize, [PushIRs | Result], Ctx);
args_to_stack([], N, Result, _) ->
	{lists:reverse(Result), N}.

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_irs(IRs, IO_Dev),
	write_irs(Rest, IO_Dev);
write_irs([{comment, Content} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t%% ~s~n", [Content]),
	write_irs(Rest, IO_Dev);
write_irs([{Tag, _} = IR | Rest], IO_Dev) when Tag =:= fn; Tag =:= label ->
	io:format(IO_Dev, "~w.~n", [IR]),
	write_irs(Rest, IO_Dev);
write_irs([IR | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~w.~n", [IR]),
	write_irs(Rest, IO_Dev);
write_irs([], _) ->
	ok.

-spec write_asm(irs(), file:io_device()) -> ok.
write_asm([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_asm(IRs, IO_Dev),
	write_asm(Rest, IO_Dev);
write_asm([{comment, Content} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t## ~s~n", [Content]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag, Name} | Rest], IO_Dev) when Tag =:= fn; Tag =:= label ->
	io:format(IO_Dev, "~s:~n", [Name]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag, Op1, Op2, Op3} | Rest], IO_Dev) when Tag =:= csrrw; Tag =:= csrrs; Tag =:= csrrc ->
	io:format(IO_Dev, "\t~s\t~s, ~s, ~s~n", [Tag, op_tag_str(Op1), op_tag_str(Op3), op_tag_str(Op2)]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag, Op1, Op2, Op3} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t~s, ~s, ~s~n", [Tag, op_tag_str(Op1), op_tag_str(Op2), op_tag_str(Op3)]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag, Op1, {{x, _} = Op2, N}} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t~s, ~w(~s)~n", [Tag, op_tag_str(Op1), N, op_tag_str(Op2)]),
	write_asm(Rest, IO_Dev);
write_asm([{label, {align, 1}, Name} | Rest], IO_Dev) ->
	io:format(IO_Dev, "~s:\t~n", [Name]),
	write_asm(Rest, IO_Dev);
write_asm([{label, {align, N}, Name} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t.balign ~w~n~s:~n", [N, Name]),
	write_asm(Rest, IO_Dev);
write_asm([{string, Content, _} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t.string\t\"~ts\"~n", [e_util:fix_special_chars(Content)]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag, Op1, Op2} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t~s, ~s~n", [Tag, op_tag_str(Op1), op_tag_str(Op2)]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag, Op1} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t~s~n", [Tag, op_tag_str(Op1)]),
	write_asm(Rest, IO_Dev);
write_asm([{Tag} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s~n", [Tag]),
	write_asm(Rest, IO_Dev);
write_asm([], _) ->
	ok.

op_tag_str(N) when is_integer(N) ->
	integer_to_list(N);
op_tag_str({x, N}) ->
	"x" ++ integer_to_list(N);
op_tag_str(Label) when is_atom(Label) ->
	Label.

%% Be careful! `smart_addi/3` will change the source register.
smart_addi(_, 0, _) ->
	[];
smart_addi(R, N, _) when ?IS_SMALL_IMMEDI(N) ->
	[{addi, R, R, N}];
smart_addi(R, N, #{free_regs := [T | _]}) ->
	[smart_li(T, N), {add, R, R, T}].

%% Be careful! `smart_li/2` will change the source register.
smart_li(R, N) when ?IS_SMALL_IMMEDI(N) ->
	[{addi, R, {x, 0}, N}];
smart_li(R, N) ->
	li_big_num(R, e_util:u_type_immedi(N)).

li_big_num(R, {0, Low}) ->
	[{addi, R, {0, 0}, Low}];
li_big_num(R, {High, 0}) ->
	[{lui, R, High}];
li_big_num(R, {High, Low}) ->
	[{lui, R, High}, {addi, R, R, Low}].

mv(R1, R2) ->
	{addi, R1, R2, 0}.

%% {x, 0} means there are not branching to generate. (already generated in previous IRs)
'br!_reg'({x, 0}, _)	-> [];
'br!_reg'(R, Label)	-> [{'br!', R, Label}].

br_reg({x, 0}, _)	-> [];
br_reg(R, Label)	-> [{'br', R, Label}].

st_tag(1) -> sb;
st_tag(_) -> sw.
ld_tag(1) -> lb;
ld_tag(_) -> lw.

%% We don't need many registers with current allocation algorithm.
%% Most RISC machine can provide 8 free registers.
-spec tmp_regs() -> [machine_reg()].
tmp_regs() ->
	[{x, 5}, {x, 6}, {x, 7}, {x, 10}, {x, 11}, {x, 12}, {x, 13}, {x, 14}].

reverse_cmp_tag('==')	-> '!=';
reverse_cmp_tag('!=')	-> '==';
reverse_cmp_tag('>=')	-> '<';
reverse_cmp_tag('<=')	-> '>';
reverse_cmp_tag('>')	-> '<=';
reverse_cmp_tag('<')	-> '>='.

to_op_normal('+')	->  add;
to_op_normal('-')	->  sub;
to_op_normal('*')	->  mul;
to_op_normal('/')	-> 'div';
to_op_normal('rem')	-> 'rem';
to_op_normal('band')	-> 'and';
to_op_normal('bor')	-> 'or';
to_op_normal('bxor')	-> 'xor';
to_op_normal('bsl')	->  sll;
to_op_normal('bsr')	->  sra.

to_op_immedi('+')	-> addi;
to_op_immedi('band')	-> andi;
to_op_immedi('bor')	-> ori;
to_op_immedi('bxor')	-> xori;
to_op_immedi('bsl')	-> slli;
to_op_immedi('bsr')	-> srai.

to_cmp_op('==')		-> beq;
to_cmp_op('!=')		-> bne;
to_cmp_op('>=')		-> bge;
to_cmp_op('<')		-> blt.

