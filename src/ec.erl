-module(ec).
-export([main/1]).

arguments() ->
	[
	#{name => input_file, short => $i, long => "-input-file", nargs => list, required => true},
	#{name => output_file, short => $o, long => "-output-file"},
	#{name => init_jump_pos, long => "-init-jump-pos", type => {integer, [{min, 0}]}},
	#{name => isr_vector_pos, long => "-v-pos", type => {integer, [{min, 0}]}},
	#{name => isr_vector_size, long => "-v-size", type => {integer, [{min, 0}]}},
	#{name => code_pos, long => "-i-pos", type => {integer, [{min, 0}]}},
	#{name => data_pos, long => "-d-pos", type => {integer, [{min, 0}]}},
	#{name => data_size, long => "-d-size", type => {integer, [{min, 0}]}},
	#{name => entry_function, long => "-entry"},
	#{name => wordsize, long => "-wordsize", type => {integer, [{min, 1}]}}
	].

handler(#{input_file := InputFile, output_file := OutputFile} = Options) ->
	PureOptions = fix_options(maps:without([input_file, output_file], Options)),
	try
		e_compiler:compile_to_machine1(InputFile, OutputFile, e_compile_option:combine(PureOptions))
	catch
		throw:Error ->
			e_util:exit_info(2, "**ERROR: ~s~n", [Error])
	end;
handler(#{input_file := InputFile} = Options) ->
	handler(Options#{output_file => InputFile ++ ".bin"}).

fix_options(#{entry_function := Entry} = Options) ->
	Options#{entry_function := list_to_atom(Entry)};
fix_options(Options) ->
	Options.

cli() ->
	#{
		arguments => lists:map(fun(#{name := Name} = M) -> M#{help => help_of(Name)} end, arguments()),
		handler => fun handler/1
	}.

-spec help_of(atom()) -> string().
help_of(input_file) ->
	"E language source file(s) to compile.";
help_of(output_file) ->
	"Output file (xx.bin) to generate.";
help_of(init_jump_pos) ->
	"Memory position for init jump.";
help_of(isr_vector_pos) ->
	"Position for interrupt vector table.";
help_of(isr_vector_size) ->
	"Size of the interrupt vector table.";
help_of(code_pos) ->
	"Position for user code.";
help_of(data_pos) ->
	"Start address for data storage";
help_of(data_size) ->
	"Size of the data storage available";
help_of(entry_function) ->
	"Entry function. (defaults to \"main\")";
help_of(wordsize) ->
	"Bit length of the target machine.";
help_of(Any) ->
	atom_to_list(Any).

main(Args) ->
	argparse:run(Args, cli(), #{progname => ec}).

