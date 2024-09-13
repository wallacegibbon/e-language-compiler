-module(e_dumper_ir1).
-export([generate_code/3]).
-include("e_record_definition.hrl").

-type irs() :: [tuple() | irs()].

-spec generate_code(e_ast(), e_ast(), string()) -> ok.
generate_code(AST, InitCode, OutputFile) ->
	IRs = [{function, '<init>'}, lists:map(fun stmt_to_ir/1, InitCode) | ast_to_ir(AST)],
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
stmt_to_ir(#e_if_stmt{condi = Condi, then = Then, 'else' = Else, loc = Loc}) ->
	{CondiIRs, R_Cond} = expr_to_ir(Condi),
	StartComment = {comment, io_lib:format("[if@~w] cond: ~s", [Loc, e_util:stmt_to_str(Condi)])},
	EndComment = {comment, io_lib:format("[if@~w] end", [Loc])},
	ThenBody = lists:map(fun(S) -> stmt_to_ir(S) end, Then),
	ElseBody = lists:map(fun(S) -> stmt_to_ir(S) end, Else),
	ThenPart = [{comment, io_lib:format("[if@~w] then part", [Loc])}, ThenBody, {goto, end_if}],
	ElsePart = [{comment, io_lib:format("[if@~w] else part", [Loc])}, {label, else_label}, ElseBody, {label, end_if}],
	[StartComment, CondiIRs, {br, R_Cond, else_label}, ThenPart, ElsePart, EndComment];
stmt_to_ir(#e_while_stmt{condi = Condi, stmts = Stmts, loc = Loc}) ->
	{CondiIRs, R_Cond} = expr_to_ir(Condi),
	StartComment = {comment, io_lib:format("[while@~w] cond: ~s", [Loc, e_util:stmt_to_str(Condi)])},
	EndComment = {comment, io_lib:format("[while@~w] end", [Loc])},
	Body = lists:map(fun(S) -> stmt_to_ir(S) end, Stmts),
	BodyPart = [{comment, io_lib:format("[while@~w] body part", [Loc])}, {label, body_start}, Body, {goto, body_start}, {label, end_while}],
	[StartComment, CondiIRs, {br, R_Cond, end_while}, BodyPart, EndComment];
stmt_to_ir(#e_return_stmt{expr = Expr}) ->
	{ExprIRs, R} = expr_to_ir(Expr),
	[ExprIRs, {return, R}];
stmt_to_ir(#e_goto_stmt{label = Label}) ->
	{goto, Label};
stmt_to_ir(#e_label{name = Label}) ->
	{label, Label};
stmt_to_ir(Stmt) ->
	{Exprs, _} = expr_to_ir(Stmt),
	Exprs.

-define(IS_ARITH(Tag),
	(
	Tag =:= '+' orelse Tag =:= '-' orelse Tag =:= '*' orelse Tag =:= '/' orelse Tag =:= 'rem' orelse
	Tag =:= 'and' orelse Tag =:= 'or' orelse Tag =:= 'band' orelse Tag =:= 'bor' orelse Tag =:= 'bxor' orelse
	Tag =:= 'bsl' orelse Tag =:= 'bsr'
	)).

-spec expr_to_ir(e_expr()) -> irs().
expr_to_ir(?OP2('=', ?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V)), Right)) ->
	{RightExprs, RTmp} = expr_to_ir(Right),
	{[RightExprs, {st_instr_from_v(V), {Name, N}, RTmp}], RTmp};
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V))) ->
	{{ld_instr_from_v(V), r_tmp, {Name, N}}, r_tmp};
expr_to_ir(?OP2(Tag, Left, Right)) when ?IS_ARITH(Tag) ->
	{IRs, {R1, R2}} = op2_to_ir_merge(Left, Right),
	{[IRs, {Tag, r_tmp, R1, R2}], r_tmp};
expr_to_ir(?I(N)) ->
	{{la, r_tmp, N}, r_tmp};
expr_to_ir(_Expr) ->
	{{}, zero}.

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

st_instr_from_v(1) -> sb;
st_instr_from_v(_) -> sw.

ld_instr_from_v(1) -> lb;
ld_instr_from_v(_) -> lw.

