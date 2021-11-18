## The E Language And Its Compiler

The E language is a simplified C language with fewer concepts and more reasonable syntax. It is designed to be:
- **E**xplicit on pointer operations.
- **E**asy to learn and implement.
- Suitable for **E**mbeded systems and friendly to **E**lectronic hobbyists.

Here are some comparisons:


### Basic Operations

|         C language         |        E language        |
|----------------------------|--------------------------|
| &p                         | p@                       |
| *p                         | p^                       |
| ***p                       | p^^^                     |
| p[3]                       | (p+3)^                   |
| p.m                        | p.m                      |
| (*p).m                     | p^.m                     |
| p->m                       | p^.m                     |
| &p[3]                      | p+3                      |
| uint8_t *p                 | p: u8^                   |
| void **p                   | p: any^^                 |
| ((struct Blah *) p)->f1    | (p as Blah^)^.f1         |
| sizeof (struct Blah *)     | sizeof(Blah^)            |
| malloc(sizeof(struct A))   | malloc(sizeof(A))        |


### Array And Struct

```
arr: {i32, 3} = {1, 2, 3};

struct Blah
    id: i32,
    name: i8^,
end

b: Blah = Blah{id = 1, name = "hello"};

c: {Blah, 2} = {Blah{id = 1, name = "a"}, Blah{id = 2, name = "b"}};
```

```c
i32 arr[3] = {1, 2, 3};

struct Blah {
    i32 id;
    char *name;
}

Blah b = {1, "hello"};

Blah c[2] = {{1, "a"}, {2, "b}};

```


### Union

The most common usage of `union` is to reuse memory, which is useful in specific situations. But most union can be simply replaced by pointer operations.
Here is an example about `union` in C language: 

```c
struct A {
    unsigned char tag;
    union {
        long long num;
        char buf[8];
    } value;
};

struct A a = {.tag = 1, .value.num = 0x12345678 };
printf("%x\n", a.value.buf[2]);
// 34

```

In E language (and also in C language), you can use pointer manipulation to achieve this:

```
struct A
    tag: u8,
    value: i64,
end

a: A = A{tag = 1, value = 0x12345678};
printf("%x\n", ((a.value@ as i8^) + 2)^);
% 34
```

To keep things minimum, E language do not support `union`.


### Enum

Enum is good, it brings better type checking to some extent. But on the other hand, everything will still work without `enum`.
The preprocessor of E language support contant definition `#define xx xxx`, which has reduced the necessity of `enum`.

To keep things minimum, E language do not support `enum`, either.


### Function Definition

```
fun main(argc: isize, argv: i8^^): isize
    return 0;
end
```

```c
int main(int argc, char **argv)
{
    return 0;
}
```


### Condition

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


### Function Pointer

```
myFunction1: fun(): fun(): fun() = anotherFuntion1;

myFunction2: fun(i8^, usize): fun(i8^, i8^): fun(isize, usize): u8^ = anotherFuntion2;

```

```c
void (*(*(*myFunction1)())())() = anotherFuntion1;

unsigned char * (*(*(*myFunction2)(char *, unsigned int))(char *, char *))(int, unsigned int) = anotherFuntion2;
```


## Why Another C ?

- The syntax of C language is bad in many situations like complex function pointers.
- The goto statement in C language is not powerful enough, gcc have "labels as values" to solve this, but it is not standard C, and it introduces new keyword.


## Editor Support

A simple Vim plugin is inside this project. Install it by copying it to the certain directory:

```sh
mkdir -p ~/.vim/pack/my/start/
cp -r ./misc/elang.vim/ ~/.vim/pack/my/start/
```


## Macro

A *token-level* macro system like C and Erlang is supported. (still under developing...)

Macro can bring many problems like overriding predefined variables. e.g. In C language, you can do dangerous things like `#define true 0`.
To solve this problem, a prefix "?" should be used to make it explicit when invoking macros. (just like erlang)


## Plans

### Target

This compiler only compile E language to C language for now, in the future,
it will be compiled directly to machine language. (RISC-V may be the only target)

### Heap And Stack

For now, `malloc` function is the only way to allocate memory on heap, the `new` keyword may be introduced so that you can create a object on heap directly.
The syntax may look like this: (not decided yet)

```
b: Blah = new Blah{id = 1, name = "hello"};

c: {Blah, 2} = new {Blah{id = 1, name = "a"}, Blah{id = 2, name = "b"}};

```

