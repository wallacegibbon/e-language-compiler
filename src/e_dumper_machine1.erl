-module(e_dumper_machine1).
-export([generate_code/2]).

-type scan_context() :: {LabelMap :: #{atom() => non_neg_integer()}, OffsetMap :: #{non_neg_integer() => [atom()]}}.

-spec generate_code([tuple()], string()) -> ok.
generate_code(IRs, OutputFile) ->
	{Instrs, {LabelMap, OffsetMap}} = scan_address(IRs, 0, [], {#{}, #{}}),
	Fn = fun(IO_Dev) -> write_instrs(encode_instr(replace_address(Instrs, LabelMap)), IO_Dev) end,
	io:format(">> LabelMap:~p~n>> OffsetMap:~p~n", [LabelMap, OffsetMap]),
	e_util:file_write(OutputFile, Fn).

-spec scan_address([tuple()], non_neg_integer(), [tuple()], scan_context()) -> {R, scan_context()} when R :: [tuple()].
scan_address([{L, Name} | Rest], Offset, Result, {LabelMap, OffsetMap}) when L =:= fn; L =:= label ->
	NewLabelMap = LabelMap#{Name => Offset},
	NewOffsetMap = maps:update_with(Offset, fun(Ns) -> [Name | Ns] end, [Name], OffsetMap),
	scan_address(Rest, Offset, Result, {NewLabelMap, NewOffsetMap});
scan_address([{la, _, _} = Orig | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + 8, [{Orig, Offset} | Result], Ctx);
scan_address([Any | Rest], Offset, Result, Ctx) ->
	scan_address(Rest, Offset + 4, [{Any, Offset} | Result], Ctx);
scan_address([], _, Result, Ctx) ->
	{lists:reverse(Result), Ctx}.

-define(IS_BR_INSTR(Tag),
	(
	Tag =:= beq orelse Tag =:= bne orelse Tag =:= bge orelse Tag =:= blt orelse Tag =:= bgt orelse Tag =:= ble
	)).

replace_address([{{Br, R1, R2, Label}, Offset} | Rest], LabelMap) when ?IS_BR_INSTR(Br) ->
	{ok, Address} = maps:find(Label, LabelMap),
	[{{Br, R1, R2, Address - Offset}, Offset} | replace_address(Rest, LabelMap)];
replace_address([{{la, R, Label}, Offset} | Rest], LabelMap) ->
	{ok, Address} = maps:find(Label, LabelMap),
	{High, Low} = e_util:u_type_immedi(Address - Offset),
	[{{auipc, R, High}, Offset}, {{addi, R, R, Low}, Offset + 4} | replace_address(Rest, LabelMap)];
replace_address([{{j, Label}, Offset} | Rest], LabelMap) ->
	{ok, Address} = maps:find(Label, LabelMap),
	if
		Address =:= Offset + 4 ->
			replace_address(Rest, LabelMap);
		true ->
			[{{j, Address - Offset}, Offset} | replace_address(Rest, LabelMap)]
	end;
replace_address([Any | Rest], LabelMap) ->
	[Any | replace_address(Rest, LabelMap)];
replace_address([], _) ->
	[].

encode_instr([{{j, Address} = I, Offset} | Rest]) ->
	[{I, <<Address:20, (0):5, (2#1101111):7>>, Offset} | encode_instr(Rest)];
encode_instr([Any | Rest]) ->
	[Any | encode_instr(Rest)];
encode_instr([]) ->
	[].

-spec write_instrs([tuple()], file:io_device()) -> ok.
write_instrs([Raw | Rest], IO_Dev) ->
	io:format(IO_Dev, "\t~w~n", [Raw]),
	write_instrs(Rest, IO_Dev);
write_instrs([], _) ->
	ok.

