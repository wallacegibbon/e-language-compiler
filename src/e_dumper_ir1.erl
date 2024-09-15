-module(e_dumper_ir1).
-export([generate_code/3]).
-include("e_record_definition.hrl").

-type irs() :: [tuple() | irs()].

-spec generate_code(e_ast(), e_ast(), string()) -> ok.
generate_code(AST, InitCode, OutputFile) ->
	IRs = [{function, '<global_var_init>'}, lists:map(fun stmt_to_ir/1, InitCode) | ast_to_ir(AST)],
	Fn = fun(IO_Dev) -> write_irs([{comment, "vim:ft=erlang"} | IRs], IO_Dev) end,
	file_transaction(OutputFile, Fn).

-spec ast_to_ir(e_ast()) -> irs().
ast_to_ir([#e_function{name = Name, stmts = Stmts} | Rest]) ->
	[{function, Name}, lists:map(fun stmt_to_ir/1, Stmts) | ast_to_ir(Rest)];
ast_to_ir([_ | Rest]) ->
	ast_to_ir(Rest);
ast_to_ir([]) ->
	[].

-spec stmt_to_ir(e_stmt()) -> irs().
stmt_to_ir(#e_if_stmt{condi = Condi, then = Then0, 'else' = Else0, loc = Loc}) ->
	{CondiIRs, R_Cond} = expr_to_ir(Condi),
	StartComment = comment('if', e_util:stmt_to_str(Condi), Loc),
	EndComment = comment('if', "end", Loc),
	Then1 = lists:map(fun(S) -> stmt_to_ir(S) end, Then0),
	Else1 = lists:map(fun(S) -> stmt_to_ir(S) end, Else0),
	Then2 = [comment('if', "then part", Loc), Then1, {goto, end_if}],
	Else2 = [comment('if', "else part", Loc), {label, else_label}, Else1, {goto, end_if}, {label, end_if}],
	[StartComment, CondiIRs, {br, R_Cond, else_label}, Then2, Else2, EndComment];
stmt_to_ir(#e_while_stmt{condi = Condi, stmts = Stmts0, loc = Loc}) ->
	{CondiIRs, R_Cond} = expr_to_ir(Condi),
	StartComment = comment(while, e_util:stmt_to_str(Condi), Loc),
	EndComment = comment(while, "end", Loc),
	Stmts1 = lists:map(fun(S) -> stmt_to_ir(S) end, Stmts0),
	Stmts2 = [comment(while, "body part", Loc), {label, body_start}, Stmts1, {goto, body_start}, {label, end_while}],
	[StartComment, CondiIRs, {br, R_Cond, end_while}, Stmts2, EndComment];
stmt_to_ir(#e_return_stmt{expr = Expr}) ->
	{ExprIRs, R} = expr_to_ir(Expr),
	[ExprIRs, {return, R}];
stmt_to_ir(#e_goto_stmt{label = Label}) ->
	[{goto, Label}];
stmt_to_ir(#e_label{name = Label}) ->
	[{label, Label}];
stmt_to_ir(Stmt) ->
	{Exprs, _} = expr_to_ir(Stmt),
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

-spec expr_to_ir(e_expr()) -> {irs(), atom()}.
expr_to_ir(?OP2('=', ?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V)), Right)) ->
	{RightIRs, R} = expr_to_ir(Right),
	{[RightIRs, {st_instr_from_v(V), {Name, N}, R}], R};
expr_to_ir(?OP2('=', ?OP2('^', Expr, ?I(V)), Right)) ->
	{RightIRs, R_R} = expr_to_ir(Right),
	{LeftIRs, R_L} = expr_to_ir(Expr),
	{[RightIRs, LeftIRs, {st_instr_from_v(V), {R_L, 0}, R_R}], r_tmp};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V))) when Name =:= '<gp>'; Name =:= '<fp>' ->
	{[{ld_instr_from_v(V), r_tmp, {Name, N}}], r_tmp};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), _)) ->
	{[{la, r_tmp, Name}, {lw, r_tmp, {r_tmp, N}}], r_tmp};
expr_to_ir(?OP2('^', Expr, ?I(V))) ->
	{IRs, R} = expr_to_ir(Expr),
	{[IRs, {ld_instr_from_v(V), r_tmp, {R, 0}}], r_tmp};
expr_to_ir(#e_op{tag = {call, Fn}, data = Args}) ->
	{FnLoadIRs, R_Fn} = expr_to_ir(Fn),
	%% TODO: prepare Args
	{[FnLoadIRs, {call, R_Fn}], r_tmp};
expr_to_ir(?OP2(Tag, Left, Right)) when ?IS_ARITH(Tag) ->
	{IRs, {R1, R2}} = op2_to_ir_merge(Left, Right),
	{[IRs, {Tag, r_tmp, R1, R2}], r_tmp};
expr_to_ir(?OP2(Tag, Left, Right)) when ?IS_COMPARE(Tag) ->
	{IRs, {R1, R2}} = op2_to_ir_merge(Left, Right),
	{[IRs, {compare_reverse(Tag), r_tmp, R1, R2}], r_tmp};
expr_to_ir(?OP1(Tag, Expr)) ->
	{IRs, R} = expr_to_ir(Expr),
	{[IRs, {Tag, r_tmp, R}], r_tmp};
expr_to_ir(?I(N)) ->
	{[{li, r_tmp, N}], r_tmp};
expr_to_ir(Expr) ->
	{[Expr], zero}.

-spec op2_to_ir_merge(e_expr(), e_expr()) -> {irs(), {atom(), atom()}}.
op2_to_ir_merge(OP1, OP2) ->
	{IRs1, R1} = expr_to_ir(OP1),
	{IRs2, R2} = expr_to_ir(OP2),
	{[IRs1, IRs2], {R1, R2}}.

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_irs(IRs, IO_Dev),
	write_irs(Rest, IO_Dev);
write_irs([{comment, Content} | Rest], IO_Dev) ->
	io:format(IO_Dev, "%% ~s~n", [Content]),
	write_irs(Rest, IO_Dev);
write_irs([{function, _} = IR | Rest], IO_Dev) ->
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

compare_reverse('==') -> '!=';
compare_reverse('!=') -> '==';
compare_reverse('>=') -> '<';
compare_reverse('<=') -> '>';
compare_reverse('>')  -> '<=';
compare_reverse('<')  -> '>='.

st_instr_from_v(1) -> sb;
st_instr_from_v(_) -> sw.

ld_instr_from_v(1) -> lb;
ld_instr_from_v(_) -> lw.

