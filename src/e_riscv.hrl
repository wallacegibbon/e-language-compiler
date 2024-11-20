-ifndef(E_RISCV_HRL).
-define(E_RISCV_HRL, 1).

%% According the calling convention of RISC-V: RA is X1, SP is X2, GP is X3, FP is X8. X5-X7 is T0-T2.
%% We use A0-A4 as T3-T7 since we pass arguments and result through stack and `Ax` are caller saved just like `Tx`.
-type machine_reg() :: {x, non_neg_integer()}.

-type irs() :: list(tuple() | irs()).
-type flatten_irs() :: list(tuple()).

-define(IS_SPECIAL_REG(Tag),
        (
          Tag =:= {x, 8} orelse Tag =:= {x, 3} orelse Tag =:= {x, 2} orelse Tag =:= {x, 1} orelse Tag =:= {x, 0}
        )).

-define(IS_IMM12(N),
        (
          N >= -2048 andalso N < 2048
        )).

-endif.
