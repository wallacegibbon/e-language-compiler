#! /bin/sh

if test $# -ne 1; then
	echo Usage: $0 xxx.bin >&2
	exit 1
fi

detail_file=$1.detail
asm_file=$1.ir1.asm

riscv64-unknown-elf-as $asm_file -o $asm_file.o
riscv64-unknown-elf-objdump -S $asm_file.o > $asm_file.o.dump

fetch_machine_code() {
	sed -n 's/^\s*[0-9a-fA-F]*:\s*\([0-9a-f]\+\).*/\1/p' $1 > $1.1
}

fetch_machine_code $asm_file.o.dump
fetch_machine_code $detail_file

echo Comparing result...
diff $detail_file.1 $asm_file.o.dump.1
echo Done.

