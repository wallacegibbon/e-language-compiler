## Load and Run

We can using OpenOCD to load and debug the generated bin file.

Start OpenOCD server:
```sh
/c/MounRiver/MounRiver_Studio/toolchain/OpenOCD/bin/openocd.exe -f C:/MounRiver/MounRiver_Studio/toolchain/OpenOCD/bin/wch-riscv.cfg
```

Then use `telnet` to connect to OpenOCD server to run load and debug commands:
```sh
telnet localhost 4444
```

Load binary file to target:
```text
program path/to/a.bin 0x00000000 reset
```

Step one instruction:
```text
step
```

Read the content of PC register:
```text
reg pc
```

Read memory (code):
```text
read_memory 0x00000000 8 32
%> 0x23 0xa0 0x1 0x0 0x97 0x2 0x0 0x0
```

```text
read_memory 0x00000190 8 32
%> 0x23 0x22 0x81 0x0 0x23 0x24 0x11 0x0
```

