## The E Language And Its Compiler

The E language is a simplified C language with fewer concepts and more reasonable syntax. It is designed to be:
- **E**asy to learn and implement.
- **E**xplicit on pointer operations.
- Suitable for **E**mbeded systems and friendly to **E**lectronic hobbyists.


Here are some comparisons:


### Basic Operations

|        C language          |     E language           |
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


### Union And Enum

E language do not support enum or union.


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

A macro system like C and Erlang (Token level) is supported (under developing...)

Macro can bring many problems (like overriding predefined variables like true and false in C), so a prefix "?" should be used
to make it explicit when invoking macros. (just like erlang)


## Plans

This compiler only compile E language to C language for now, in the future,
it will be compiled directly to machine language. (RISC-V may be the only target)

