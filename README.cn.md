# “E编程语言”

[English](./README.md)

“E语言”是一个简化版的“C语言”，它有着更少的概念，更合理的语法，它的设计理念是：
1. 指针操作更“显式”（**E**xplicit）
2. 容易使用和实现（**E**asy）
3. 适合嵌入式系统（**E**mbedded systems），适合电子爱好者（**E**lectronic hobbyists）

以下是C语言和E语言的一些对比：

## 基本操作

|           C语言             |           E语言             |
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

> 为了“显式”操作，在E语言中，`p + 1` 不像C语言中表示 `p + 1 * N`，如果要达到C语言中的效果，需要写成 `p + 1 * N`。

## 数组和结构体

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
int32_t arr[3] = {1, 2, 3};

struct Blah {
	int id;
	char *name;
}

struct Blah b = {1, "hello"};

struct Blah c[2] = {{1, "a"}, {2, "b"}};
```

E语言的结构体字段的声明，可以包含默认值，你可以这样写：

```
struct Blah
	id: word = 1;
	name: byte^ = "default_name_string";
end
```

> 在E语言中，结构体的字段，和变量定义有着完全相同的写法。（在C语言中，它们的写法只是相似并不相同）


## 联合体

C语言中“联合体”主要的用途，是复用内存，这个在特定使用场景下比较方便，但是同时，联合体操作可以简单被类型转换加指针操作替换，举个例子：

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

在E语言中（或者C语言中），你可以通过下列指针操作取代联合体：

```
struct A
	tag: byte;
	value: word;
end

a: A = A{tag = 1, value = 0x12345678};
printf("%x\n", (a.value@ + 2)^);
%> 34
```

为了保证语言更加简单，E语言此处做了取舍，放弃了对联合体的支持。


## 枚举

枚举是很好的概念，它带来了更强的类型检测，但同时，没有枚举并不会带来实质性的功能缺失。为了保证语言更加简单，E语言也放弃了对枚举的支持。


## 函数定义

```
fn main(argc: word; argv: byte^^): word
	return 0;
end
```

```c
int main(int argc, char **argv)
{
	return 0;
}
```


## 分支

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


## 循环

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


## 函数指针

```
my_fn1: fn(): fn(): fn() = another_fn1;

my_fn2: fn(byte^; word): fn(byte^; byte^): fn(word; word): byte^ = another_fn2;

```

```c
void (*(*(*my_fn1)())())() = another_fn1;

char * (*(*(*my_fn2)(char *, int))(char *, char *))(int, int) = another_fn2;
```


# 编辑器支持

本项目带有一个简单的Vim插件，在项目目录运行下面的命令可以进行安装：

```sh
mkdir -p ~/.vim/pack/my/start/
cp -r ./misc/elang.vim/ ~/.vim/pack/my/start/
```

