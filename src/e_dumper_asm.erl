-module(e_dumper_asm).
-export([generate_code/2]).

-type irs() :: [tuple() | irs()].
-type instrs() :: [tuple() | instrs()].

-spec generate_code(irs(), string()) -> ok.
generate_code(IRs, OutputFile) ->
	Instrs = lists:map(fun translate_ir/1, IRs),
	Fn = fun(IO_Dev) -> write_instrs([{comment, "vim:ft=asm"} | Instrs], IO_Dev) end,
	e_util:file_write(OutputFile, Fn).

translate_ir(Any) ->
	Any.

-spec write_instrs(instrs(), file:io_device()) -> ok.
write_instrs([IRs | Rest], IO_Dev) when is_list(IRs) ->
	write_instrs(IRs, IO_Dev),
	write_instrs(Rest, IO_Dev);
write_instrs([{comment, Content} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t;; ~s~n", [Content]),
	write_instrs(Rest, IO_Dev);
write_instrs([{label, Name} | Rest], IO_Dev) ->
	io:format(IO_Dev, "~s:~n", [Name]),
	write_instrs(Rest, IO_Dev);
write_instrs([{fn, Name} | Rest], IO_Dev) ->
	io:format(IO_Dev, "~s:~n", [Name]),
	write_instrs(Rest, IO_Dev);
write_instrs([{LdStOP, Rd, {R, Offset}} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t\t~w, ~w, ~w~n", [LdStOP, Rd, R, Offset]),
	write_instrs(Rest, IO_Dev);
write_instrs([{OP, Rd, R1, R2} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t\t~w, ~w, ~w~n", [OP, Rd, R1, R2]),
	write_instrs(Rest, IO_Dev);
write_instrs([{OP, R1, R2} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t\t~w, ~w~n", [OP, R1, R2]),
	write_instrs(Rest, IO_Dev);
write_instrs([{OP, R} | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~s\t\t~w~n", [OP, R]),
	write_instrs(Rest, IO_Dev);
write_instrs([], _) ->
	ok.

