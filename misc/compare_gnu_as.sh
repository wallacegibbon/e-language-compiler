#! /bin/sh

set -e
if test $# -ne 1; then
    echo Usage: $0 xxx.code >&2
    exit 1
fi

detail_file=$1.detail.txt
asm_file=$1.asm

riscv64-unknown-elf-as $asm_file -o $asm_file.o
riscv64-unknown-elf-ld $asm_file.o -o $asm_file.elf
riscv64-unknown-elf-objdump -D $asm_file.elf \
| sed -z 's/\(.*\)Disassembly of section \.riscv\.attributes:.*/\1/' \
> $asm_file.dump

fetch_machine_code() {
    sed -n 's/^\s*[0-9a-fA-F]*:\s*\([0-9a-f]\+\).*/\1/p' $1
}

## Drop interrupt vector table in detail file
sed -z 's/.*\n\(\s\+__init:.*\)/\1/' $detail_file > $detail_file.0

fetch_machine_code $detail_file.0 > $detail_file.1
fetch_machine_code $asm_file.dump > $asm_file.dump.1

echo Comparing result...
## If there are something the detail_file has while asm_file.dump.1 don't have, it's an error.
diff $detail_file.1 $asm_file.dump.1 | grep '^[^>]'
echo Done.

rm $detail_file.* $asm_file.*
