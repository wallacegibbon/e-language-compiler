ecompiler
=====

This is the compiler for my programming language (I didn't give it a name yet).
For convenience, I will call it E language in this document.


## What is it ?

E language is a simplified C language, they have similar semantics, but the
syntax of E language is more compact.

Here are some comparisons:

### basic operations

```
&p						p@
*p						p^
***p						p^^^
p[3]						(p+3)^
p.m						p.m
p->m						p^.m
&p[3]						p+3
uint8_t *p					p: u8^
void **p					p: any^^
sizeof (struct Blah *)				sizeof(Blah^)
malloc(sizeof(struct A))			malloc(size(A))
```

### array and struct

```
arr: {i32, 3} = {1, 2, 3};

struct Blah
    id: i32,
    name: i8^,
end

b: Blah = Blah{id=1, name="hello"};

c: {Blah, 2} = { Blah{id=1, name="a"}, Blah{id=2, name="b"} };
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

### function definition

```
fun main(argc: u8, argv: i8^^): i32
    return 0;
end
```

```c
int main(int argc, char **argv) {
    return 0;
}
```

### condition

```
if(fn1(fn2(val1)) >= fn3(val2))
    fn4();
elif(val3)
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

### function pointer

```
myfn1: fun(): fun(): fun(u8^) = get_incfn1;
```

```c
void (*(*(*myfn1)())())(u8* );

myfn1 = get_incfn1;
```

more docs are to be written...

