-module(ec).
-export([main/1]).

cli() -> #{

arguments =>
[
	#{name => input_file, short => $i, long => "-input-file", nargs => list, required => true, help => help_of(input_file)},
	#{name => output_file, short => $o, long => "-output-file", help => help_of(output_file)},
	#{name => init_jump_pos, long => "-init-jump-pos", type => {integer, [{min, 0}]}, help => help_of(init_jump_pos)},
	#{name => isr_vector_pos, long => "-v-pos", type => {integer, [{min, 0}]}, help => help_of(isr_vector_pos)},
	#{name => isr_vector_size, long => "-v-size", type => {integer, [{min, 0}]}, help => help_of(isr_vector_size)},
	#{name => code_pos, long => "-i-pos", type => {integer, [{min, 0}]}, help => help_of(code_pos)},
	#{name => data_pos, long => "-d-pos", type => {integer, [{min, 0}]}, help => help_of(data_pos)},
	#{name => data_size, long => "-d-size", type => {integer, [{min, 0}]}, help => help_of(data_size)},
	#{name => entry_function, long => "-entry", help => help_of(entry_function)},
	#{name => wordsize, long => "-wordsize", type => {integer, [{min, 1}]}, help => help_of(wordsize)}
],

handler =>
fun
Handler(#{input_file := InputFile, output_file := OutputFile} = Options) ->
	PureOptions = fix_options(maps:without([input_file, output_file], Options)),
	try
		e_compiler:compile_to_machine1(InputFile, OutputFile, e_compile_option:combine(PureOptions))
	catch
		throw:Error ->
			io:format(standard_error, "**ERROR: ~s~n", [Error]),
			erlang:halt(2)
	end;
Handler(#{input_file := InputFile} = Options) ->
	Handler(Options#{output_file => InputFile ++ ".bin"})
end
}.

fix_options(#{entry_function := Entry} = Options) ->
	Options#{entry_function := list_to_atom(Entry)};
fix_options(Options) ->
	Options.

-spec help_of(atom()) -> string().
help_of(input_file) ->
	"The E language source file(s) to compile.";
help_of(output_file) ->
	"The output file (xx.bin) to generate.";
help_of(init_jump_pos) ->
	"A memory position for init jump.";
help_of(isr_vector_pos) ->
	"The position for interrupt vector table.";
help_of(isr_vector_size) ->
	"The size of the interrupt vector table.";
help_of(code_pos) ->
	"The position for user code.";
help_of(data_pos) ->
	"The start address for data storage";
help_of(data_size) ->
	"The size of the data storage available";
help_of(entry_function) ->
	"The entry function. (defaults to \"main\")";
help_of(wordsize) ->
	"The bit length of the target machine.";
help_of(Any) ->
	atom_to_list(Any).

main(Args) ->
	argparse:run(Args, cli(), #{progname => ec}).

