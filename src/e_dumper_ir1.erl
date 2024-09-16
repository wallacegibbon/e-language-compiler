-module(e_dumper_ir1).
-export([generate_code/4]).
-include("e_record_definition.hrl").

-type irs() :: [tuple() | irs()].
-type context() :: {PointerWidth :: pos_integer()}.

-spec generate_code(e_ast(), e_ast(), string(), context()) -> ok.
generate_code(AST, InitCode, OutputFile, Ctx) ->
	IRs = [{fn, '<init1>'}, lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, InitCode) | ast_to_ir(AST, Ctx)],
	Fn = fun(IO_Dev) -> write_irs([{comment, "vim:ft=erlang"} | IRs], IO_Dev) end,
	file_transaction(OutputFile, Fn).

-spec ast_to_ir(e_ast(), context()) -> irs().
ast_to_ir([#e_function{name = Name, vars = #e_vars{size = Size0}, stmts = Stmts} | Rest], {PointerWidth} = Ctx) ->
	Size1 = e_util:fill_unit_pessi(Size0, PointerWidth),
	StackOpIRs = [{li, t1, Size1}, {'-', t2, sp, t1}, {mv, fp, sp}, {mv, sp, t2}],
	[{fn, Name}, StackOpIRs, lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Stmts) | ast_to_ir(Rest, Ctx)];
ast_to_ir([_ | Rest], Ctx) ->
	ast_to_ir(Rest, Ctx);
ast_to_ir([], _) ->
	[].

-spec stmt_to_ir(e_stmt(), context()) -> irs().
stmt_to_ir(#e_if_stmt{condi = Condi, then = Then0, 'else' = Else0, loc = Loc}, Ctx) ->
	{CondiIRs, R_Cond} = expr_to_ir(Condi, Ctx),
	StartComment = comment('if', e_util:stmt_to_str(Condi), Loc),
	EndComment = comment('if', "end", Loc),
	Then1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Then0),
	Else1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Else0),
	Then2 = [comment('if', "then part", Loc), Then1, {j, end_if}],
	Else2 = [comment('if', "else part", Loc), {label, else_label}, Else1, {j, end_if}, {label, end_if}],
	[StartComment, CondiIRs, {jcond, R_Cond, else_label}, Then2, Else2, EndComment];
stmt_to_ir(#e_while_stmt{condi = Condi, stmts = Stmts0, loc = Loc}, Ctx) ->
	{CondiIRs, R_Cond} = expr_to_ir(Condi, Ctx),
	StartComment = comment(while, e_util:stmt_to_str(Condi), Loc),
	EndComment = comment(while, "end", Loc),
	Stmts1 = lists:map(fun(S) -> stmt_to_ir(S, Ctx) end, Stmts0),
	Stmts2 = [comment(while, "body part", Loc), {label, body_start}, Stmts1, {j, body_start}, {label, end_while}],
	[StartComment, CondiIRs, {jcond, R_Cond, end_while}, Stmts2, EndComment];
stmt_to_ir(#e_return_stmt{expr = Expr}, Ctx) ->
	{ExprIRs, R} = expr_to_ir(Expr, Ctx),
	[ExprIRs, {return, R}];
stmt_to_ir(#e_goto_stmt{label = Label}, _) ->
	[{j, Label}];
stmt_to_ir(#e_label{name = Label}, _) ->
	[{label, Label}];
stmt_to_ir(Stmt, Ctx) ->
	{Exprs, _} = expr_to_ir(Stmt, Ctx),
	Exprs.

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

-spec expr_to_ir(e_expr(), context()) -> {irs(), atom()}.
expr_to_ir(?OP2('=', ?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V)), Right), Ctx) ->
	{RightIRs, R} = expr_to_ir(Right, Ctx),
	{[RightIRs, {st_instr_from_v(V), {Name, N}, R}], R};
expr_to_ir(?OP2('=', ?OP2('^', Expr, ?I(V)), Right), Ctx) ->
	{RightIRs, R_R} = expr_to_ir(Right, Ctx),
	{LeftIRs, R_L} = expr_to_ir(Expr, Ctx),
	{[RightIRs, LeftIRs, {st_instr_from_v(V), {R_L, 0}, R_R}], tn};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V)), _) when Name =:= gp; Name =:= fp ->
	{[{ld_instr_from_v(V), tn, {Name, N}}], tn};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), _), _) ->
	{[{la, tn, Name}, {lw, tn, {tn, N}}], tn};
expr_to_ir(?OP2('^', Expr, ?I(V)), Ctx) ->
	{IRs, R} = expr_to_ir(Expr, Ctx),
	{[IRs, {ld_instr_from_v(V), tn, {R, 0}}], tn};
expr_to_ir(#e_op{tag = {call, Fn}, data = Args}, {PointerWidth} = Ctx) ->
	ArgPreparingIRs = args_to_stack(Args, PointerWidth, Ctx),
	{FnLoadIRs, R_Fn} = expr_to_ir(Fn, Ctx),
	{[ArgPreparingIRs, FnLoadIRs, {call, R_Fn}], tn};
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_ARITH(Tag) ->
	{IRs, {R1, R2}} = op2_to_ir_merge(Left, Right, Ctx),
	{[IRs, {Tag, tn, R1, R2}], tn};
expr_to_ir(?OP2(Tag, Left, Right), Ctx) when ?IS_COMPARE(Tag) ->
	{IRs, {R1, R2}} = op2_to_ir_merge(Left, Right, Ctx),
	{[IRs, {e_util:reverse_compare_tag(Tag), tn, R1, R2}], tn};
expr_to_ir(?OP1(Tag, Expr), Ctx) ->
	{IRs, R} = expr_to_ir(Expr, Ctx),
	{[IRs, {Tag, tn, R}], tn};
expr_to_ir(#e_varref{name = Name}, _) ->
	{[], Name};
expr_to_ir(#e_string{value = Value}, _) ->
	{[{la, tn, Value}], tn};
expr_to_ir(?I(N), _) ->
	{[{li, tn, N}], tn};
expr_to_ir(?F(N), _) ->
	%% TODO: float is special
	{[{li, tn, N}], tn}.

args_to_stack([Arg | Rest], N, {PointerWidth} = Ctx) ->
	{IRs, R} = expr_to_ir(Arg, Ctx),
	[IRs, {sw, {sp, -N}, R} | args_to_stack(Rest, N + PointerWidth, Ctx)];
args_to_stack([], _, _) ->
	[].

-spec op2_to_ir_merge(e_expr(), e_expr(), context()) -> {irs(), {atom(), atom()}}.
op2_to_ir_merge(OP1, OP2, Ctx) ->
	{IRs1, R1} = expr_to_ir(OP1, Ctx),
	{IRs2, R2} = expr_to_ir(OP2, Ctx),
	{[IRs1, IRs2], {R1, R2}}.

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_irs(IRs, IO_Dev),
	write_irs(Rest, IO_Dev);
write_irs([{comment, Content} | Rest], IO_Dev) ->
	io:format(IO_Dev, "%% ~s~n", [Content]),
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

