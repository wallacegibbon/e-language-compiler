-module(ec).
-export([main/1]).

cli() -> #{

arguments =>
[
	#{name => input_file, short => $i, long => "-input-file", required => true},
	#{name => output_file, short => $o, long => "-output-file"},
	#{name => init_jump_pos, long => "-init-jump-pos", type => {integer, [{min, 0}]}},
	#{name => isr_vector_pos, long => "-v-pos", type => {integer, [{min, 0}]}},
	#{name => isr_vector_size, long => "-v-size", type => {integer, [{min, 0}]}},
	#{name => code_pos, long => "-i-pos", type => {integer, [{min, 0}]}},
	#{name => data_pos, long => "-d-pos", type => {integer, [{min, 0}]}},
	#{name => data_size, long => "-d-size", type => {integer, [{min, 0}]}},
	#{name => entry_function, long => "-entry", type => atom},
	#{name => wordsize, long => "-wordsize", type => {integer, [{min, 1}]}}
],

handler =>
fun
Handler(#{input_file := InputFile, output_file := OutputFile} = Options) ->
	PureOptions = maps:without([input_file, output_file], Options),
	try
		e_compiler:compile_to_machine1(InputFile, OutputFile, PureOptions)
	catch
		throw:Error ->
			io:format(standard_error, "**ERROR: ~s~n", [Error]),
			erlang:halt(2)
	end;
Handler(#{input_file := InputFile} = Options) ->
	Handler(Options#{output_file => InputFile ++ ".bin"})
end
}.

main(Args) ->
	argparse:run(Args, cli(), #{progname => ec}).

