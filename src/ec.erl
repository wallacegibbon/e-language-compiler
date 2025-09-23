-module(ec).
-export([main/1]).

arguments() ->
  [
   #{name => input_file, short => $i, long => "-input-file", nargs => list, required => true},
   #{name => output_file, short => $o, long => "-output-file"},
   #{name => code_pos, long => "-c-pos", type => {custom, fun parse_address_num/1}},
   #{name => data_pos, long => "-d-pos", type => {custom, fun parse_address_num/1}},
   #{name => data_size, long => "-d-size", type => {custom, fun parse_size_num/1}},
   #{name => ivec_pos, long => "-v-pos", type => {custom, fun parse_address_num/1}},
   #{name => ivec_size, long => "-v-size", type => {custom, fun parse_size_num/1}},
   #{name => ivec_init_jump, long => "-v-init-jump", type => boolean},
   #{name => prefer_shift, long => "-prefer-shift", type => boolean},
   #{name => entry_function, long => "-entry", type => {custom, fun list_to_atom/1}, default => main},
   #{name => wordsize, long => "-wordsize", type => {integer, [4, 8]}}
  ].

handler(#{input_file := InputFile, output_file := OutputFile} = Options) ->
  MergedOptions = e_compile_option:combine(maps:without([input_file, output_file], Options)),
  try
    e_compiler:compile_to_machine1(InputFile, OutputFile, MergedOptions)
  catch
    throw:Error ->
      e_util:exit_info(2, "**ERROR: ~s~n", [Error])
  end;
handler(#{input_file := InputFile} = Options) ->
  handler(Options#{output_file => InputFile ++ ".bin"}).

parse_size_num(NumString) ->
  parse_size_num(string:uppercase(NumString), [], 1).

parse_size_num([N | Rest], Collected, 1) when N >= $0, N =< $9 ->
  parse_size_num(Rest, [N | Collected], 1);
parse_size_num([$_ | Rest], Collected, 1) ->
  parse_size_num(Rest, Collected, 1);
parse_size_num("GB", Collected, 1) ->
  parse_size_num([], Collected, 1024 * 1024 * 1024);
parse_size_num("G", Collected, 1) ->
  parse_size_num([], Collected, 1024 * 1024 * 1024);
parse_size_num("MB", Collected, 1) ->
  parse_size_num([], Collected, 1024 * 1024);
parse_size_num("M", Collected, 1) ->
  parse_size_num([], Collected, 1024 * 1024);
parse_size_num("KB", Collected, 1) ->
  parse_size_num([], Collected, 1024);
parse_size_num("K", Collected, 1) ->
  parse_size_num([], Collected, 1024);
parse_size_num([], Collected, Unit) ->
  list_to_integer(lists:reverse(Collected)) * Unit.

parse_address_num([$0, $x | Nums]) ->
  parse_address_num(Nums, 16);
parse_address_num([$0, $o | Nums]) ->
  parse_address_num(Nums, 8);
parse_address_num([$0, $b | Nums]) ->
  parse_address_num(Nums, 2);
parse_address_num(Nums) ->
  parse_address_num(Nums, 10).

parse_address_num(Nums, Base) ->
  list_to_integer([C || C <- Nums, C =/= $_], Base).

cli() ->
  #{
    arguments => [M#{help => help_of(Name)} || #{name := Name} = M <- arguments()],
    handler => fun handler/1
   }.

-spec help_of(atom()) -> string().
help_of(input_file) ->
  "E language source file(s) to compile.";
help_of(output_file) ->
  "Filename for output file(s).";
help_of(code_pos) ->
  "Position for user code.";
help_of(data_pos) ->
  "Start address for data storage";
help_of(data_size) ->
  "Size of the data storage available";
help_of(ivec_pos) ->
  "Position for interrupt vector table.";
help_of(ivec_size) ->
  "Size of the interrupt vector table.";
help_of(ivec_init_jump) ->
  "Jump as the first element of interrupt vector table.";
help_of(prefer_shift) ->
  "Use shift instead of mul when possible.";
help_of(entry_function) ->
  "Entry function.";
help_of(wordsize) ->
  "Bit length of the target machine.".

main(Args) ->
  argparse:run(Args, cli(), #{progname => ec}).
