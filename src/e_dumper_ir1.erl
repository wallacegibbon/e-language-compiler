-module(e_dumper_ir1).
-export([generate_code/4]).
-include("e_record_definition.hrl").

-type context() :: #{wordsize => pos_integer(), scope_tag => atom(), tmp_regs => [atom()], free_regs => [atom()]}.
-type irs() :: [tuple() | irs()].

-spec generate_code(e_ast(), e_ast(), string(), non_neg_integer()) -> ok.
generate_code(AST, InitCode, OutputFile, WordSize) ->
	Regs = tmp_regs(),
	Ctx = #{wordsize => WordSize, scope_tag => top, tmp_regs => Regs, free_regs => Regs},
	IRs = [{fn, '<init1>'}, lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, InitCode) | ast_to_ir(AST, WordSize)],
	Fn = fun(IO_Dev) -> write_irs([{comment, "vim:ft=erlang"} | IRs], IO_Dev) end,
	file_transaction(OutputFile, Fn).

-spec ast_to_ir(e_ast(), non_neg_integer()) -> irs().
ast_to_ir([#e_function{name = Name, stmts = Stmts} = Fn | Rest], WordSize) ->
	#e_function{vars = #e_vars{size = Size0}} = Fn,
	Size1 = e_util:fill_unit_pessi(Size0, WordSize),
	%% The extra `2` is for `fp` and `returning address`.
	FrameSize = Size1 + WordSize * 2,
	SaveRegs = [{sw, fp, {sp, Size1}}, {sw, ra, {sp, Size1 + WordSize}}, {mv, fp, sp}],
	Regs = tmp_regs(),
	[T1 | _] = Regs,
	PrepareFrame = [{li, T1, FrameSize}, {'+', sp, sp, T1}],
	Prologue = [{comment, prologue_start}, SaveRegs, PrepareFrame, {comment, prologue_end}],
	RestoreRegs = [{mv, sp, fp}, {lw, fp, {sp, Size1}}, {lw, ra, {sp, Size1 + WordSize}}],
	EndLabel = {label, generate_tag(Name, epilogue)},
	Epilogue = [{comment, epilogue_start}, EndLabel, RestoreRegs, {ret, ra}, {comment, epilogue_end}],
	Ctx = #{wordsize => WordSize, scope_tag => Name, tmp_regs => Regs, free_regs => Regs},
	Body = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Stmts),
	[{fn, Name}, Prologue, Body, Epilogue | ast_to_ir(Rest, WordSize)];
ast_to_ir([_ | Rest], Ctx) ->
	ast_to_ir(Rest, Ctx);
ast_to_ir([], _) ->
	[].

-spec stmt_to_ir(e_stmt(), context()) -> irs().
stmt_to_ir(#e_if_stmt{condi = Condi, then = Then0, 'else' = Else0, loc = Loc}, #{scope_tag := ScopeTag} = Ctx) ->
	{CondiIRs, R_Cond, _} = expr_to_ir(Condi, Ctx),
	StartComment = comment('if', e_util:stmt_to_str(Condi), Loc),
	EndComment = comment('if', "end", Loc),
	Then1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Then0),
	Else1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Else0),
	ElseLabel = generate_tag(ScopeTag, 'else', Loc),
	EndLabel = generate_tag(ScopeTag, if_end, Loc),
	Then2 = [comment('if', "then part", Loc), Then1, {j, EndLabel}],
	Else2 = [comment('if', "else part", Loc), {label, ElseLabel}, Else1, {label, EndLabel}],
	[StartComment, CondiIRs, {br, R_Cond, ElseLabel}, Then2, Else2, EndComment];
stmt_to_ir(#e_while_stmt{condi = Condi, stmts = Stmts0, loc = Loc}, #{scope_tag := ScopeTag} = Ctx) ->
	{CondiIRs, R_Cond, _} = expr_to_ir(Condi, Ctx),
	StartComment = comment(while, e_util:stmt_to_str(Condi), Loc),
	EndComment = comment(while, "end", Loc),
	StartLabel = generate_tag(ScopeTag, while_start, Loc),
	EndLabel = generate_tag(ScopeTag, while_end, Loc),
	RawBody = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Stmts0),
	Body = [comment(while, "body part", Loc), RawBody, {j, StartLabel}, {label, EndLabel}],
	[StartComment, {label, StartLabel}, CondiIRs, {br, R_Cond, EndLabel}, Body, EndComment];
stmt_to_ir(#e_return_stmt{expr = Expr}, #{scope_tag := ScopeTag} = Ctx) ->
	{ExprIRs, R, _} = expr_to_ir(Expr, Ctx),
	%% We use stack to pass result
	[ExprIRs, {comment, "prepare return value"}, {sw, R, {fp, 0}}, {j, generate_tag(ScopeTag, epilogue)}];
stmt_to_ir(#e_goto_stmt{label = Label}, #{scope_tag := ScopeTag}) ->
	[{j, generate_tag(ScopeTag, Label)}];
stmt_to_ir(#e_label{name = Label}, #{scope_tag := ScopeTag}) ->
	[{label, generate_tag(ScopeTag, Label)}];
stmt_to_ir(Stmt, Ctx) ->
	{Exprs, _, _} = expr_to_ir(Stmt, Ctx),
	Exprs.

generate_tag(ScopeTag, Tag, {Line, Column}) ->
	list_to_atom(e_util:fmt("~s_~s_~w_~w", [ScopeTag, Tag, Line, Column])).

generate_tag(ScopeTag, Tag) ->
	list_to_atom(e_util:fmt("~s_~s", [ScopeTag, Tag])).

comment(Tag, Info, {Line, Col}) ->
	{comment, io_lib:format("[~s@~w:~w] ~s", [Tag, Line, Col, Info])}.

-define(IS_ARITH(Tag),
	(
	Tag =:= '+' orelse Tag =:= '-' orelse Tag =:= '*' orelse Tag =:= '/' orelse Tag =:= 'rem' orelse
	Tag =:= 'and' orelse Tag =:= 'or' orelse Tag =:= 'band' orelse Tag =:= 'bor' orelse Tag =:= 'bxor' orelse
	Tag =:= 'bsl' orelse Tag =:= 'bsr'
	)).

-define(IS_COMPARE(Tag),
	(
	Tag =:= '>' orelse Tag =:= '<' orelse Tag =:= '==' orelse Tag =:= '!=' orelse
	Tag =:= '>=' orelse Tag =:= '<='
	)).

-define(IS_SPECIAL_REG(Tag),
	(
	Tag =:= fp orelse Tag =:= gp orelse Tag =:= zero
	)).

-spec expr_to_ir(e_expr(), context()) -> {irs(), atom(), context()}.
expr_to_ir(?OP2('=', ?OP2('^', ?OP2('+', #e_varref{} = Varref, ?I(N)), ?I(V)), Right), Ctx) ->
	{RightIRs, R1, Ctx1} = expr_to_ir(Right, Ctx),
	{VarrefIRs, R2, Ctx2} = expr_to_ir(Varref, Ctx1),
	{[RightIRs, VarrefIRs, {st_instr_from_v(V), {R2, N}, R1}], R1, Ctx2};
expr_to_ir(?OP2('=', ?OP2('^', Expr, ?I(V)), Right), Ctx) ->
	{RightIRs, R1, Ctx1} = expr_to_ir(Right, Ctx),
	{LeftIRs, R2, Ctx2} = expr_to_ir(Expr, Ctx1),
	{[RightIRs, LeftIRs, {st_instr_from_v(V), {R2, 0}, R1}], R1, Ctx2};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{} = Varref, ?I(N)), ?I(V)), Ctx) ->
	{IRs, R, #{free_regs := RestRegs}} = expr_to_ir(Varref, Ctx),
	[T1 | RestRegs2] = RestRegs,
	{[IRs, {ld_instr_from_v(V), T1, {R, N}}], T1, Ctx#{free_regs := recycle_tmpreg([R], RestRegs2)}};
expr_to_ir(?OP2('^', Expr, ?I(V)), Ctx) ->
	{IRs, R, #{free_regs := RestRegs}} = expr_to_ir(Expr, Ctx),
	[T1 | RestRegs2] = RestRegs,
	{[IRs, {ld_instr_from_v(V), T1, {R, 0}}], T1, Ctx#{free_regs := recycle_tmpreg([R], RestRegs2)}};
expr_to_ir(#e_op{tag = {call, Fn}, data = Args}, #{wordsize := WordSize} = Ctx) ->
	#{free_regs := FreeRegs, tmp_regs := TmpRegs} = Ctx,
	UsedRegs = TmpRegs -- FreeRegs,
	OffsetForUsedRegs = length(UsedRegs) * WordSize,
	StoreIRs = e_util:list_map(fun(R, I) -> {sw, R, {sp, I * WordSize}} end, UsedRegs),
	StackOP1 = {'+', sp, sp, OffsetForUsedRegs},
	RestoreIRs = e_util:list_map(fun(R, I) -> {lw, R, {sp, I * WordSize}} end, UsedRegs),
	StackOP2 = {'-', sp, sp, OffsetForUsedRegs},
	BeforeIRs = [{comment, e_util:fmt("regs to save (before call): ~w", [UsedRegs])}, StoreIRs, StackOP1],
	AfterIRs = [{comment, e_util:fmt("regs to restore (after call): ~w", [UsedRegs])}, StackOP2, RestoreIRs],
	%% The calling related steps
	ArgPreparingIRs = args_to_stack(Args, 0, Ctx),
	{FnLoadIRs, R, Ctx1} = expr_to_ir(Fn, Ctx),
	LoadRet = [{comment, "load returned value"}, {lw, R, {sp, 0}}],
	{[{comment, "push args"}, ArgPreparingIRs, FnLoadIRs, BeforeIRs, {jalr, ra, R}, LoadRet, AfterIRs], R, Ctx1};
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_ARITH(Tag) ->
	{IRs1, R1, Ctx1} = expr_to_ir(Left, Ctx),
	{IRs2, R2, #{free_regs := RestRegs}} = expr_to_ir(Right, Ctx1),
	[T1 | RestRegs2] = RestRegs,
	{[IRs1, IRs2, {Tag, T1, R1, R2}], T1, Ctx#{free_regs := recycle_tmpreg([R2, R1], RestRegs2)}};
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_COMPARE(Tag) ->
	{IRs1, R1, Ctx1} = expr_to_ir(Left, Ctx),
	{IRs2, R2, #{free_regs := RestRegs}} = expr_to_ir(Right, Ctx1),
	[T1 | RestRegs2] = RestRegs,
	ReversedTag = e_util:reverse_compare_tag(Tag),
	{[IRs1, IRs2, {ReversedTag, T1, R1, R2}], T1, Ctx#{free_regs := recycle_tmpreg([R2, R1], RestRegs2)}};
expr_to_ir(?OP1(Tag, Expr), Ctx) ->
	{IRs, R, Ctx1} = expr_to_ir(Expr, Ctx),
	{[IRs, {Tag, R}], R, Ctx1};
expr_to_ir(#e_varref{name = Name}, Ctx) when ?IS_SPECIAL_REG(Name) ->
	{[], Name, Ctx};
expr_to_ir(#e_varref{name = Name}, #{free_regs := [R | RestRegs]} = Ctx) ->
	{[{la, R, Name}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(#e_string{value = Value}, #{free_regs := [R | RestRegs]} = Ctx) ->
	%% TODO: string literals should be placed in certain place.
	{[{la, R, Value}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(?I(N), #{free_regs := Regs} = Ctx) ->
	[R | RestRegs] = Regs,
	{[{li, R, N}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(?F(N), #{free_regs := Regs} = Ctx) ->
	%% TODO: float is special
	[R | RestRegs] = Regs,
	{[{li, R, N}], R, Ctx#{free_regs := RestRegs}};
expr_to_ir(Any, _) ->
	e_util:ethrow(element(2, Any), "IR1: unsupported expr \"~w\"", [Any]).

recycle_tmpreg([R | Regs], RegBank) when ?IS_SPECIAL_REG(R) ->
	recycle_tmpreg(Regs, RegBank);
recycle_tmpreg([R | Regs], RegBank) ->
	recycle_tmpreg(Regs, [R | RegBank]);
recycle_tmpreg([], RegBank) ->
	RegBank.

args_to_stack([Arg | Rest], N, #{wordsize := WordSize} = Ctx) ->
	{IRs, R, _} = expr_to_ir(Arg, Ctx),
	[IRs, {sw, R, {sp, N}} | args_to_stack(Rest, N + WordSize, Ctx)];
args_to_stack([], _, _) ->
	[].

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_irs(IRs, IO_Dev),
	write_irs(Rest, IO_Dev);
write_irs([{comment, Content} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t%% ~s~n", [Content]),
	write_irs(Rest, IO_Dev);
write_irs([{fn, _} = IR | Rest], IO_Dev) ->
	io:format(IO_Dev, "~w.~n", [IR]),
	write_irs(Rest, IO_Dev);
write_irs([IR | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~w.~n", [IR]),
	write_irs(Rest, IO_Dev);
write_irs([], _) ->
	ok.

-spec file_transaction(string(), fun((file:io_device()) -> R)) -> R when R :: ok.
file_transaction(Filename, Handle) ->
	{ok, IO_Dev} = file:open(Filename, [write]),
	try
		Handle(IO_Dev)
	after
		ok = file:close(IO_Dev)
	end.

st_instr_from_v(1) -> sb;
st_instr_from_v(_) -> sw.
ld_instr_from_v(1) -> lb;
ld_instr_from_v(_) -> lw.

%% We don't need many registers with current allocation algorithm.
%% Most RISC machine can provide 8 free registers.
tmp_regs() ->
	[t0, t1, t2, t3, t4, t5, t6, t7].

