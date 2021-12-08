-module(eAssembler).
-export([compileFile/1]).

compileFile(FileName) ->
    AssemblyCodeList = file:consult(FileName),
    ok.

assembleOP({addi, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({stli, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({stliu, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({andi, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({ori, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({xori, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({slli, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({srli, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;
assembleOP({srai, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;

assembleOP({add, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({sub, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({stl, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({stlu, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({'and', {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({'or', {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({'xor', {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({sll, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({srl, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;
assembleOP({sra, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
    <<>>;

assembleOP({beq, {r, Rs1}, {r, Rs2}, Label}) ->
    <<>>;
assembleOP({bne, {r, Rs1}, {r, Rs2}, Label}) ->
    <<>>;
assembleOP({blt, {r, Rs1}, {r, Rs2}, Label}) ->
    <<>>;
assembleOP({bltu, {r, Rs1}, {r, Rs2}, Label}) ->
    <<>>;
assembleOP({bge, {r, Rs1}, {r, Rs2}, Label}) ->
    <<>>;
assembleOP({bgeu, {r, Rs1}, {r, Rs2}, Label}) ->
    <<>>;

assembleOP({lw, {r, Rd}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({lh, {r, Rd}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({lhu, {r, Rd}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({lb, {r, Rd}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({lbu, {r, Rd}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({sw, {r, Rs2}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({sh, {r, Rs2}, {r, Rs1}, Offset}) ->
    <<>>;
assembleOP({sb, {r, Rs2}, {r, Rs1}, Offset}) ->
    <<>>;

assembleOP({csrrw, {r, Rd}, {csr, Rcsr}, {r, Rs1}}) ->
    <<>>;
assembleOP({csrrs, {r, Rd}, {csr, Rcsr}, {r, Rs1}}) ->
    <<>>;
assembleOP({csrrc, {r, Rd}, {csr, Rcsr}, {r, Rs1}}) ->
    <<>>;
assembleOP({csrrwi, {r, Rd}, {csr, Rcsr}, ImmediateInteger}) ->
    <<>>;
assembleOP({csrrsi, {r, Rd}, {csr, Rcsr}, ImmediateInteger}) ->
    <<>>;
assembleOP({csrrci, {r, Rd}, {csr, Rcsr}, ImmediateInteger}) ->
    <<>>;

assembleOP({auipc, {r, Rd}, Offset}) ->
    <<>>;
assembleOP({lui, {r, Rd}, Offset}) ->
    <<>>;

assembleOP({jal, {r, Rd}, Label}) ->
    <<>>;
assembleOP({jalr, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
    <<>>;

assembleOP(Any) ->
    throw({invalidOPCode, Any}).

disassembleOP(<<>>) ->
    {}.

decodeRegisterName(zero) -> {r, 0};
decodeRegisterName(ra) -> {r, 1};
decodeRegisterName(sp) -> {r, 2};
decodeRegisterName(gp) -> {r, 3};
decodeRegisterName(tp) -> {r, 4};
decodeRegisterName(t0) -> {r, 5};
decodeRegisterName(t1) -> {r, 6};
decodeRegisterName(t2) -> {r, 7};
decodeRegisterName(s0) -> {r, 8};
decodeRegisterName(fp) -> {r, 8};
decodeRegisterName(s1) -> {r, 9};
decodeRegisterName(a0) -> {r, 10};
decodeRegisterName(a1) -> {r, 11};
decodeRegisterName(a2) -> {r, 12};
decodeRegisterName(a3) -> {r, 13};
decodeRegisterName(a4) -> {r, 14};
decodeRegisterName(a5) -> {r, 15};
decodeRegisterName(a6) -> {r, 16};
decodeRegisterName(a7) -> {r, 17};
decodeRegisterName(s2) -> {r, 18};
decodeRegisterName(s3) -> {r, 19};
decodeRegisterName(s4) -> {r, 20};
decodeRegisterName(s5) -> {r, 21};
decodeRegisterName(s6) -> {r, 22};
decodeRegisterName(s7) -> {r, 23};
decodeRegisterName(s8) -> {r, 24};
decodeRegisterName(s9) -> {r, 25};
decodeRegisterName(s10) -> {r, 26};
decodeRegisterName(s11) -> {r, 27};
decodeRegisterName(t3) -> {r, 28};
decodeRegisterName(t4) -> {r, 29};
decodeRegisterName(t5) -> {r, 30};
decodeRegisterName(t6) -> {r, 31};
decodeRegisterName(ft0) -> {f, 0};
decodeRegisterName(ft1) -> {f, 1};
decodeRegisterName(ft2) -> {f, 2};
decodeRegisterName(ft3) -> {f, 3};
decodeRegisterName(ft4) -> {f, 4};
decodeRegisterName(ft5) -> {f, 5};
decodeRegisterName(ft6) -> {f, 6};
decodeRegisterName(ft7) -> {f, 7};
decodeRegisterName(fs0) -> {f, 8};
decodeRegisterName(fs1) -> {f, 9};
decodeRegisterName(fa0) -> {f, 10};
decodeRegisterName(fa1) -> {f, 11};
decodeRegisterName(fa2) -> {f, 12};
decodeRegisterName(fa3) -> {f, 13};
decodeRegisterName(fa4) -> {f, 14};
decodeRegisterName(fa5) -> {f, 15};
decodeRegisterName(fa6) -> {f, 16};
decodeRegisterName(fa7) -> {f, 17};
decodeRegisterName(fs2) -> {f, 18};
decodeRegisterName(fs3) -> {f, 19};
decodeRegisterName(fs4) -> {f, 20};
decodeRegisterName(fs5) -> {f, 21};
decodeRegisterName(fs6) -> {f, 22};
decodeRegisterName(fs7) -> {f, 23};
decodeRegisterName(fs8) -> {f, 24};
decodeRegisterName(fs9) -> {f, 25};
decodeRegisterName(fs10) -> {f, 26};
decodeRegisterName(fs11) -> {f, 27};
decodeRegisterName(ft8) -> {f, 28};
decodeRegisterName(ft9) -> {f, 29};
decodeRegisterName(ft10) -> {f, 30};
decodeRegisterName(ft11) -> {f, 31};
decodeRegisterName(Any) -> Any.
