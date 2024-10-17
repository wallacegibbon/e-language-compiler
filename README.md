# The E Programming Language

[中文](./README.cn.md)

The `E Language` is like a simplified `C language` with fewer concepts and more reasonable syntax. It is designed to be:
1. `E`xplicit on pointer operations.
2. `E`asy to learn and implement.
3. Designed for `E`mbedded systems and `E`lectronic hobbyists.

Here are some comparisons of basic operations of C language and E language:

## Basic Operations

|            C language           |           E language            |
|---------------------------------|---------------------------------|
| &**p**                          | **p**@                          |
| \***p**                         | **p**^                          |
| \*\*\***p**                     | **p**^^^                        |
| **p**[3]                        | **p**[3]                        |
| &**p**[3]                       | **p**[3]@                       |
| **p** + 3                       | **p**[3]@                       |
| (char\*)**p** + 1               | **p** + 1                       |
| **p**.m                         | **p**.m                         |
| (\***p**).m                     | **p**^.m                        |
| **p**-\>m                       | **p**^.m                        |
| char \***p**                    | **p**: byte^                    |
| void \*\***p**                  | **p**: any^^                    |
| ((struct Blah \*) **p**)-\>f1   | **p** as (Blah^)^.f1            |

> In C language, `p[2]` is the same as `*(p + 2 * sizeof(*p))`, which is a waste.
> In E language, `p + 2` doesn't mean `p + 2 * N` like C language, it is just `p + 2`, while `p[2]` is the same as `p[2]` in C language.


## Array And Struct

In E language:
```text
arr: {word, 3} = {1, 2, 3};

struct Blah
	id: word;
	name: byte^;
end

b: Blah = Blah{id = 1, name = "hello"};

c: {Blah, 2} = {Blah{id = 1, name = "a"}, Blah{id = 2, name = "b"}};
```

In C language:
```c
int arr[3] = {1, 2, 3};

struct Blah {
	int id;
	char *name;
};

struct Blah b = {1, "hello"};

struct Blah c[2] = {{1, "a"}, {2, "b"}};
```


## Union

The most common usage of `union` is to reuse memory, which is convenient in specific situations. But union can also be simply replaced by pointer operations.

Here is an example about `union` in C language: 

```c
struct A {
	char tag;
	union {
		long long num;
		char buf[8];
	} value;
};

struct A a = {.tag = 1, .value.num = 0x12345678 };
printf("%x\n", a.value.buf[2]);
//> 34
```

In E language (and also in C language), we can use pointer manipulation to achieve this:

```text
struct A
	tag: byte;
	value: word;
end

a: A = A{tag = 1, value = 0x12345678};
printf("%x\n", (a.value@ + 2) as (byte^)^);
%> 34
```

To keep things minimum, E language do not support `union`.


## Enum

Enum is good, it brings better type checking to some extent. But on the other hand, everything will still work without `enum`.

To keep things minimum, E language do not support `enum`, either.


## Function Definition

```text
fn main(argc: word, argv: byte^^): word
	return 0;
end
```

```c
int main(int argc, char **argv)
{
	return 0;
}
```


## Condition

```text
if fn1(fn2(val1)) >= fn3(val2) then
	fn4();
elif val3 > 100 then
	fn5();
else
	fn6();
end
```

```c
if (fn1(fn2(val1)) >= fn3(val2)) {
	fn4();
} else if (val3) {
	fn5();
} else {
	fn6();
}
```


## Loop


```text
while test() do
	do_something();
end
```

```c
while (test()) {
	do_something();
}
```


## Function Pointer

```text
my_fn1: fn(): fn(): fn() = another_fn1;

my_fn2: fn(byte^; word): fn(byte^; byte^): fn(word; word): byte^ = another_fn2;

```

```c
void (*(*(*my_fn1)())())() = another_fn1;

char * (*(*(*my_fn2)(char *, int))(char *, char *))(int, int) = another_fn2;
```


## Interrupt

For embedded systems, interrupt subroutine is important, to define an ISR:

```text
interrupt(26)
fn exti_isr()
	%% Clear interrupt flag.
	exti4^.INTF = 0b10000;

	%...
end
```

The `26` indicates the interrupt ID which can be found in the chip documentation.

When writting C code, users usually need to read/write assembly files and linkers files to make ISR work.
We don't need those things in E language, we can write ISRs as long as we have the chip document.
This is one of the features that make E language friendly to electronic hobbyists.


## The `void` Type

In C language, there is a type called `void` which is used for 2 purpose:
1. For function definition, `void` indicates the function do not have parameters or return value;
2. For pointers, `void*` stands for a pointer who can point to any type.

> The logic of C language is: When we dereferncing `void*`, we will got a `void` type who can not be part of an expression, so some usage bugs can be found by this design. This is not the best design since many C programmers do not understand this logic, they just remembered the rule.

In E language, there is no `void` type exposed to users.

For function definitions, we do not use `void`. When we define functions without parameters or return value, we just left them out.

For pointers, we use `any` instead. `any^` in E language is same as `void*` in C language.

> In C language, we can not just left out parameters for historical reasons: no parameter and void have different meanings. We always need a `void` to make our code robust.


## Boolean Expression

In C language, any expression can act as a boolean expression, which have resulted in many many wrong code.

People always write wrong code like this:
```c
	if (a = b) {
		//...
	}
```

In E language, there are only 6 boolean expression: `>`, `>=`, `<`, `<=`, `!=`, `==`.

So the following code will refused by the compiler:
```text
	if a = b then
		%...
	end
```

Compile error:
```text
./sample/led_sample_1.e:115:9: invalid boolean expression for if
```


## Bitwise Operations

Since we use `^` as pointer operator (`^` looks like a pointer), we can not follow the C style. (C use `^` as xor)

Instead, E language choose the Erlang style for bit operations.

E language defined keywords `band`, `bor`, `bnot`, `bxor` for bitwise logic operations, which is just like `&`, `|`, `~`, `^` in C language.

And `bsl`, `bsr` for shift operations, which is just like `<<`, `>>` in C language.

Bitwise and shift operations are important, but they are not as common as pointer operations, this is the main reason E language choose `^` as pointer operator.


# The Compiler

The compiler compiles E language source file to RISC-V (32bit RV32I/RV32E) machine code directly.

To call the compiler, we can call the command line tool `ec`.

Example for CH32V307:
```sh
ec -i ./sample/ch32v.e ./sample/led_sample_1.e -o /tmp/a --v-pos 0 --v-size 416 --c-pos 416 --d-pos 0x2000_0000 --d-size 64K --v-init-jump
```

Example for CH32V003:
```sh
ec -i ./sample/ch32v.e ./sample/led_sample_2.e -o /tmp/a --v-pos 0 --v-size 156 --c-pos 156 --d-pos 0x2000_0000 --d-size 2K --v-init-jump --prefer-shift
```

We will get 2 bin files: `a.code.bin` (our code) and `a.ivec.bin` (interrupt vector table).
Then write them into the right address.

If you are an Erlang user, you can also call the compiler from erlang shell:
```erlang
e_compiler:compile_to_machine1(["./sample/ch32v.e", "./sample/led_sample_1.e"], "/tmp/a", #{...}).
```


# Editor Support

A simple Vim plugin is inside this project. Install it by copying it to the certain directory:

```sh
mkdir -p ~/.vim/pack/my/start/
cp -r ./misc/elang.vim/ ~/.vim/pack/my/start/
```

Then add the following configurations to `~/.vimrc`:
```vim
autocmd BufRead,BufNewFile *.e setlocal filetype=elang
```

