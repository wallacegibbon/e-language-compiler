-module(e_dumper_ir1).
-export([generate_code/3]).
-include("e_record_definition.hrl").

-type irs() :: [tuple() | irs()].

-spec generate_code(e_ast(), e_ast(), string()) -> ok.
generate_code(AST, InitCode, OutputFile) ->
	IRs = [{function, '<init>'}, lists:map(fun statement_to_ir/1, InitCode) | ast_to_ir(AST)],
	Fn = fun(IO_Dev) -> write_irs(IRs, IO_Dev) end,
	file_transaction(OutputFile, Fn).

-spec ast_to_ir(e_ast()) -> irs().
ast_to_ir([#e_function{name = Name, stmts = Stmts} | Rest]) ->
	[{function, Name}, lists:map(fun statement_to_ir/1, Stmts) | ast_to_ir(Rest)];
ast_to_ir([_ | Rest]) ->
	ast_to_ir(Rest);
ast_to_ir([]) ->
	[].

-spec statement_to_ir(e_stmt()) -> irs().
statement_to_ir(?OP2('=', ?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V)), Right)) ->
	{RightExprs, RTmp} = expr_to_ir(Right),
	[RightExprs, {st_instr_from_v(V), {Name, N}, RTmp}];
statement_to_ir(Stmt) ->
	expr_to_ir(Stmt).

-spec expr_to_ir(e_expr()) -> irs().
expr_to_ir(?OP2('^', ?OP2('+', #e_varref{name = Name}, ?I(N)), ?I(V))) ->
	{{ld_instr_from_v(V), r_tmp, {Name, N}}, r_tmp};
expr_to_ir(?I(N)) when N > 0, N < 2048 ->
	{{la, r_tmp, N}, r_tmp};
expr_to_ir(_Expr) ->
	{{}, zero}.

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_irs(IRs, IO_Dev),
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
	{ok, IO_Dev} = file:open(Filename, [read, write]),
	try
		Handle(IO_Dev)
	after
		ok = file:close(IO_Dev)
	end.

st_instr_from_v(1) -> sb;
st_instr_from_v(_) -> sw.

ld_instr_from_v(1) -> lb;
ld_instr_from_v(_) -> lw.

