# The E Programming Language

[中文](./README.cn.md)

The E language is like a simplified C language with fewer concepts and more reasonable syntax. It is designed to be:
1. **E**xplicit on pointer operations.
2. **E**asy to learn and implement.
3. Suitable for **E**mbedded systems and friendly to **E**lectronic hobbyists.

Here are some comparisons of C language and E language:

## Basic Operations

|          C language         |         E language          |
|-----------------------------|-----------------------------|
| &p                          | p@                          |
| \*p                         | p^                          |
| \*\*\*p                     | p^^^                        |
| &p[3]                       | p + 3 * N                   |
| p[3]                        | (p + 3 * N)^                |
| p.m                         | p.m                         |
| (\*p).m                     | p^.m                        |
| p-\>m                       | p^.m                        |
| char \*p                    | p: byte^                    |
| void \*\*p                  | p: any^^                    |
| ((struct Blah \*) p)-\>f1   | p as (Blah^)^.f1            |
| sizeof(struct Blah \*)      | sizeof(Blah^)               |

> To achieve the 1st goal (explicit on pointer operations), `p + 1` does NOT mean `p + 1 * N` like C language. We need to write `p + 1 * N` explicitly.

## Array And Struct

```
arr: {word, 3} = {1, 2, 3};

struct Blah
	id: word;
	name: byte^;
end

b: Blah = Blah{id = 1, name = "hello"};

c: {Blah, 2} = {Blah{id = 1, name = "a"}, Blah{id = 2, name = "b"}};
```

```c
int arr[3] = {1, 2, 3};

struct Blah {
	int id;
	char *name;
}

struct Blah b = {1, "hello"};

struct Blah c[2] = {{1, "a"}, {2, "b"}};
```

The `struct` in E language support default value. You can write:

```
struct Blah
	id: word = 1;
	name: byte^ = "default_name_string";
end
```

> In `E` langauge, fields in struct have the same form as variables. (In C language, they only have similar forms, not the same)


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

In E language (and also in C language), you can use pointer manipulation to achieve this:

```
struct A
	tag: byte;
	value: word;
end

a: A = A{tag = 1, value = 0x12345678};
printf("%x\n", (a.value@ + 2)^);
%> 34
```

To keep things minimum, E language do not support `union`.


## Enum

Enum is good, it brings better type checking to some extent. But on the other hand, everything will still work without `enum`.

To keep things minimum, E language do not support `enum`, either.


## Function Definition

```
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

```
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


```
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

```
my_fn1: fn (): fn (): fn () = another_fn1;

my_fn2: fn (byte^; word): fn (byte^; byte^): fn (word; word): byte^ = another_fn2;

```

```c
void (*(*(*my_fn1)())())() = another_fn1;

char * (*(*(*my_fn2)(char *, int))(char *, char *))(int, int) = another_fn2;
```


# Editor Support

A simple Vim plugin is inside this project. Install it by copying it to the certain directory:

```sh
mkdir -p ~/.vim/pack/my/start/
cp -r ./misc/elang.vim/ ~/.vim/pack/my/start/
```

