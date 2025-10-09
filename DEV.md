# For CH32Vxxx MCU

## Load and Run

We can using OpenOCD to load and debug the generated bin file.

Download bin file to the target machine:
```sh
openocd.exe -f wch-riscv.cfg \
  -c "program a.code.bin 0x00000000 program a.ivec.bin 0x0000FC00 verify reset exit"
```

Start OpenOCD server:
```sh
openocd.exe -f wch-riscv.cfg
```

Then use `telnet` to connect to OpenOCD server to run load and debug commands:
```sh
telnet localhost 4444
```

Load binary file to target:
```sh
program path/to/a.code.bin 0x00000000 program a.ivec.bin 0x0000FC00 reset
```

Step one instruction:
```sh
step
```

Read the content of PC register:
```sh
reg pc
```

Read memory (code):
```sh
read_memory 0x00000000 8 32
```

The output of this command should be like:
```
0x23 0xa0 0x1 0x0 0x97 0x2 0x0 0x0
```

```sh
read_memory 0x00000190 8 32
```

The output of this command should be like:
```
0x23 0x22 0x81 0x0 0x23 0x24 0x11 0x0
```
