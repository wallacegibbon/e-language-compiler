-module(e_dumper_ir1).
-export([generate_code/3]).
-include("e_record_definition.hrl").

-spec generate_code(e_ast(), e_ast(), string()) -> ok.
generate_code(AST, InitCode, OutputFile) ->
	Fn = fun(IO_Dev) -> write_irs(statements_to_ir(AST, InitCode), IO_Dev) end,
	file_transaction(OutputFile, Fn).

-spec statements_to_ir(e_ast(), [e_stmt()]) -> iolist().
statements_to_ir([#e_function{name = main, stmts = Stmts} | Rest], InitCode) ->
	[{} | statements_to_ir(Rest, InitCode)];
statements_to_ir([#e_function{name = Name, stmts = Stmts} | Rest], InitCode) ->
	[{} | statements_to_ir(Rest, InitCode)];
statements_to_ir([_ | Rest], InitCode) ->
	statements_to_ir(Rest, InitCode);
statements_to_ir([], _) ->
	[].

-type irs() :: [tuple() | irs()].

-spec write_irs(irs(), file:io_device()) -> ok.
write_irs([IRs | Rest], Tail, IO_Dev) when is_list(IRs) ->
	write_irs(IRs, write_irs(Rest, Tail, IO_Dev), IO_Dev);
write_irs([IR | Rest], Tail, IO_Dev) ->
	io:format(IO_Dev, "~w.~n", [IR]),
	write_irs(Rest, Tail, IO_Dev);
write_irs([], Tail, _) ->
	Tail.

-spec file_transaction(string(), fun((file:io_device()) -> R)) -> R when R :: ok.
file_transaction(Filename, Handle) ->
	{ok, IO_Dev} = file:open(Filename, [read, write]),
	try
		Handle(IO_Dev)
	after
		ok = file:close(IO_Dev)
	end.

