-module(e_riscv_ir).
-export([tmp_regs/0, addi/3, li/2, mv/2, to_op_normal/1, to_op_immedi/1, to_cmp_op/1]).
-include("e_riscv.hrl").

%% We don't need many temporary registers with current allocation algorithm, 8 is more than enough.
%% And by avoiding {x, 15~31}, our code can run on a RV32E machine who have only 16 registers.
-spec tmp_regs() -> [machine_reg()].
tmp_regs() ->
    [{x, 5}, {x, 6}, {x, 7}, {x, 10}, {x, 11}, {x, 12}, {x, 13}, {x, 14}].

-spec addi(machine_reg(), integer(), machine_reg()) -> irs().
addi(R, N, _) when ?IS_SMALL_IMMEDI(N) ->
    [{addi, R, R, N}];
addi(R, N, T) ->
    [li(T, N), {add, R, R, T}].

li(R, N) when ?IS_SMALL_IMMEDI(N) ->
    [{addi, R, {x, 0}, N}];
li(R, N) ->
    li_u(R, e_util:u_type_immedi(N)).

li_u(R, {0, L}) -> [{addi, R, {x, 0}, L}];
li_u(R, {H, 0}) -> [{lui, R, H}];
li_u(R, {H, L}) -> [{lui, R, H}, {addi, R, R, L}].

mv(R1, R2) ->
    [{addi, R1, R2, 0}].

to_op_normal('+'   ) -> 'add';
to_op_normal('-'   ) -> 'sub';
to_op_normal('*'   ) -> 'mul';
to_op_normal('/'   ) -> 'div';
to_op_normal('rem' ) -> 'rem';
to_op_normal('band') -> 'and';
to_op_normal('bor' ) -> 'or' ;
to_op_normal('bxor') -> 'xor';
to_op_normal('bsl' ) -> 'sll';
to_op_normal('bsr' ) -> 'sra'.

to_op_immedi('+'   ) -> addi;
to_op_immedi('band') -> andi;
to_op_immedi('bor' ) -> ori ;
to_op_immedi('bxor') -> xori;
to_op_immedi('bsl' ) -> slli;
to_op_immedi('bsr' ) -> srai.

to_cmp_op('==') -> beq;
to_cmp_op('!=') -> bne;
to_cmp_op('>=') -> bge;
to_cmp_op('<' ) -> blt.
