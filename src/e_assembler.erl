-module(e_assembler).
-export([compile_file/1]).

compile_file(FileName) ->
	AssemblyCodeList = file:consult(FileName),
	ok.

assemble_op({addi, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({stli, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({stliu, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({andi, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({ori, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({xori, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({slli, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({srli, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({srai, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op({add, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({sub, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({stl, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({stlu, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({'and', {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({'or', {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({'xor', {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({sll, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({srl, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({sra, {r, Rd}, {r, Rs1}, {r, Rs2}}) ->
	<<>>;
assemble_op({beq, {r, Rs1}, {r, Rs2}, Label}) ->
	<<>>;
assemble_op({bne, {r, Rs1}, {r, Rs2}, Label}) ->
	<<>>;
assemble_op({blt, {r, Rs1}, {r, Rs2}, Label}) ->
	<<>>;
assemble_op({bltu, {r, Rs1}, {r, Rs2}, Label}) ->
	<<>>;
assemble_op({bge, {r, Rs1}, {r, Rs2}, Label}) ->
	<<>>;
assemble_op({bgeu, {r, Rs1}, {r, Rs2}, Label}) ->
	<<>>;
assemble_op({lw, {r, Rd}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({lh, {r, Rd}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({lhu, {r, Rd}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({lb, {r, Rd}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({lbu, {r, Rd}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({sw, {r, Rs2}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({sh, {r, Rs2}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({sb, {r, Rs2}, {r, Rs1}, Offset}) ->
	<<>>;
assemble_op({csrrw, {r, Rd}, {csr, Rcsr}, {r, Rs1}}) ->
	<<>>;
assemble_op({csrrs, {r, Rd}, {csr, Rcsr}, {r, Rs1}}) ->
	<<>>;
assemble_op({csrrc, {r, Rd}, {csr, Rcsr}, {r, Rs1}}) ->
	<<>>;
assemble_op({csrrwi, {r, Rd}, {csr, Rcsr}, ImmediateInteger}) ->
	<<>>;
assemble_op({csrrsi, {r, Rd}, {csr, Rcsr}, ImmediateInteger}) ->
	<<>>;
assemble_op({csrrci, {r, Rd}, {csr, Rcsr}, ImmediateInteger}) ->
	<<>>;
assemble_op({auipc, {r, Rd}, Offset}) ->
	<<>>;
assemble_op({lui, {r, Rd}, Offset}) ->
	<<>>;
assemble_op({jal, {r, Rd}, Label}) ->
	<<>>;
assemble_op({jalr, {r, Rd}, {r, Rs1}, ImmediateInteger}) ->
	<<>>;
assemble_op(Any) ->
	throw({invalid_op_code, Any}).

disassemble_op(<<>>) ->
	{}.

decode_reg_name(zero) ->
	{r, 0};
decode_reg_name(ra) ->
	{r, 1};
decode_reg_name(sp) ->
	{r, 2};
decode_reg_name(gp) ->
	{r, 3};
decode_reg_name(tp) ->
	{r, 4};
decode_reg_name(t0) ->
	{r, 5};
decode_reg_name(t1) ->
	{r, 6};
decode_reg_name(t2) ->
	{r, 7};
decode_reg_name(s0) ->
	{r, 8};
decode_reg_name(fp) ->
	{r, 8};
decode_reg_name(s1) ->
	{r, 9};
decode_reg_name(a0) ->
	{r, 10};
decode_reg_name(a1) ->
	{r, 11};
decode_reg_name(a2) ->
	{r, 12};
decode_reg_name(a3) ->
	{r, 13};
decode_reg_name(a4) ->
	{r, 14};
decode_reg_name(a5) ->
	{r, 15};
decode_reg_name(a6) ->
	{r, 16};
decode_reg_name(a7) ->
	{r, 17};
decode_reg_name(s2) ->
	{r, 18};
decode_reg_name(s3) ->
	{r, 19};
decode_reg_name(s4) ->
	{r, 20};
decode_reg_name(s5) ->
	{r, 21};
decode_reg_name(s6) ->
	{r, 22};
decode_reg_name(s7) ->
	{r, 23};
decode_reg_name(s8) ->
	{r, 24};
decode_reg_name(s9) ->
	{r, 25};
decode_reg_name(s10) ->
	{r, 26};
decode_reg_name(s11) ->
	{r, 27};
decode_reg_name(t3) ->
	{r, 28};
decode_reg_name(t4) ->
	{r, 29};
decode_reg_name(t5) ->
	{r, 30};
decode_reg_name(t6) ->
	{r, 31};
decode_reg_name(ft0) ->
	{f, 0};
decode_reg_name(ft1) ->
	{f, 1};
decode_reg_name(ft2) ->
	{f, 2};
decode_reg_name(ft3) ->
	{f, 3};
decode_reg_name(ft4) ->
	{f, 4};
decode_reg_name(ft5) ->
	{f, 5};
decode_reg_name(ft6) ->
	{f, 6};
decode_reg_name(ft7) ->
	{f, 7};
decode_reg_name(fs0) ->
	{f, 8};
decode_reg_name(fs1) ->
	{f, 9};
decode_reg_name(fa0) ->
	{f, 10};
decode_reg_name(fa1) ->
	{f, 11};
decode_reg_name(fa2) ->
	{f, 12};
decode_reg_name(fa3) ->
	{f, 13};
decode_reg_name(fa4) ->
	{f, 14};
decode_reg_name(fa5) ->
	{f, 15};
decode_reg_name(fa6) ->
	{f, 16};
decode_reg_name(fa7) ->
	{f, 17};
decode_reg_name(fs2) ->
	{f, 18};
decode_reg_name(fs3) ->
	{f, 19};
decode_reg_name(fs4) ->
	{f, 20};
decode_reg_name(fs5) ->
	{f, 21};
decode_reg_name(fs6) ->
	{f, 22};
decode_reg_name(fs7) ->
	{f, 23};
decode_reg_name(fs8) ->
	{f, 24};
decode_reg_name(fs9) ->
	{f, 25};
decode_reg_name(fs10) ->
	{f, 26};
decode_reg_name(fs11) ->
	{f, 27};
decode_reg_name(ft8) ->
	{f, 28};
decode_reg_name(ft9) ->
	{f, 29};
decode_reg_name(ft10) ->
	{f, 30};
decode_reg_name(ft11) ->
	{f, 31};
decode_reg_name(Any) ->
	Any.

