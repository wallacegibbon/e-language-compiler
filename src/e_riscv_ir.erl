-module(e_riscv_ir).
-export([tmp_regs/0, smart_addi/3, smart_li/2, mv/2, to_op_normal/1, to_op_immedi/1, to_cmp_op/1]).
-include("e_riscv.hrl").

%% We don't need many registers with current allocation algorithm.
%% Most RISC machine can provide 8 free registers.
-spec tmp_regs() -> [machine_reg()].
tmp_regs() ->
	[{x, 5}, {x, 6}, {x, 7}, {x, 10}, {x, 11}, {x, 12}, {x, 13}, {x, 14}].

%% Be careful! `smart_addi/3` will change the source register.
-spec smart_addi(machine_reg(), integer(), machine_reg()) -> irs().
smart_addi(_, 0, _) ->
	[];
smart_addi(R, N, _) when ?IS_SMALL_IMMEDI(N) ->
	[{addi, R, R, N}];
smart_addi(R, N, T) ->
	[smart_li(T, N), {add, R, R, T}].

%% Be careful! `smart_li/2` will change the source register.
smart_li(R, N) when ?IS_SMALL_IMMEDI(N) ->
	[{addi, R, {x, 0}, N}];
smart_li(R, N) ->
	li_big_num(R, e_util:u_type_immedi(N)).

li_big_num(R, {0, Low}) ->
	[{addi, R, {0, 0}, Low}];
li_big_num(R, {High, 0}) ->
	[{lui, R, High}];
li_big_num(R, {High, Low}) ->
	[{lui, R, High}, {addi, R, R, Low}].

mv(R1, R2) ->
	[{addi, R1, R2, 0}].

to_op_normal('+')	->  add;
to_op_normal('-')	->  sub;
to_op_normal('*')	->  mul;
to_op_normal('/')	-> 'div';
to_op_normal('rem')	-> 'rem';
to_op_normal('band')	-> 'and';
to_op_normal('bor')	-> 'or';
to_op_normal('bxor')	-> 'xor';
to_op_normal('bsl')	->  sll;
to_op_normal('bsr')	->  sra.

to_op_immedi('+')	-> addi;
to_op_immedi('band')	-> andi;
to_op_immedi('bor')	-> ori;
to_op_immedi('bxor')	-> xori;
to_op_immedi('bsl')	-> slli;
to_op_immedi('bsr')	-> srai.

to_cmp_op('==')		-> beq;
to_cmp_op('!=')		-> bne;
to_cmp_op('>=')		-> bge;
to_cmp_op('<')		-> blt.

