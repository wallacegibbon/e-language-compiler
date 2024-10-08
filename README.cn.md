# “E编程语言”

[English](./README.md)

“E语言”是一个简化版的“C语言”，它有着更少的概念，更合理的语法，它的设计理念是：
1. 指针操作更“显式”（`E`xplicit）
2. 容易使用和实现（`E`asy）
3. 专注嵌入式系统（`E`mbedded systems），适合电子爱好者（`E`lectronic hobbyists）


以下是C语言和E语言在基本操作上的一些对比：

## 基本操作

|           C语言             |           E语言             |
|-----------------------------|-----------------------------|
| &p                          | p@                          |
| \*p                         | p^                          |
| \*\*\*p                     | p^^^                        |
| &p[3]                       | p[3]@                       |
| p[3]                        | p[3]                        |
| (char\*)p + 1               | p + 1                       |
| p.m                         | p.m                         |
| (\*p).m                     | p^.m                        |
| p-\>m                       | p^.m                        |
| char \*p                    | p: byte^                    |
| void \*\*p                  | p: any^^                    |
| ((struct Blah \*) p)-\>f1   | p as (Blah^)^.f1            |
| sizeof(struct Blah \*)      | sizeof(Blah^)               |

> 在C语言中，数组访问和指针操作完全等价，即`p[2]`完全等价于`*(p + 2 * sizeof(*p))`，语义上是一种浪费。在E语言中，`p + 2` 不像C语言中表示 `p + 2 * N`，而是单纯表达 `p + 2`，而`p[2]`则等同于C语言中的`p[2]`，方便了使用。


## 数组和结构体

E语言：
```text
arr: {word, 3} = {1, 2, 3};

struct Blah
	id: word;
	name: byte^;
end

b: Blah = Blah{id = 1, name = "hello"};

c: {Blah, 2} = {Blah{id = 1, name = "a"}, Blah{id = 2, name = "b"}};
```

C语言：
```c
int32_t arr[3] = {1, 2, 3};

struct Blah {
	int id;
	char *name;
}

struct Blah b = {1, "hello"};

struct Blah c[2] = {{1, "a"}, {2, "b"}};
```


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

```text
struct A
	tag: byte;
	value: word;
end

a: A = A{tag = 1, value = 0x12345678};
printf("%x\n", (a.value@ + 2) as (byte^)^);
%> 34
```

为了保证语言更加简单，E语言此处做了取舍，放弃了对联合体的支持。


## 枚举

枚举是很好的概念，它带来了更强的类型检测，但同时，没有枚举并不会带来实质性的功能缺失。为了保证语言更加简单，E语言也放弃了对枚举的支持。


## 函数定义

```text
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


## 循环

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


## 函数指针

```text
my_fn1: fn(): fn(): fn() = another_fn1;

my_fn2: fn(byte^; word): fn(byte^; byte^): fn(word; word): byte^ = another_fn2;

```

```c
void (*(*(*my_fn1)())())() = another_fn1;

char * (*(*(*my_fn2)(char *, int))(char *, char *))(int, int) = another_fn2;
```


## 中断

对于嵌入式系统，中断处理非常重要和基础，在E语言里，要定义一个ISR，只需要直接写：

```text
interrupt(26)
fn exti_isr()
	%% Clear interrupt flag.
	exti4^.INTF = 0b10000;

	%...
end
```

这里的`26`表示的是中断的ID，这个ID可以直接在芯片手册中查找到。

用C语言的时候，用户往往需要写汇编代码和链接脚本来配合ISR，比较麻烦，E语言省去了这层麻烦。
只需要芯片手册，就能轻松编写中断相关的代码，这是E语言对电子爱好者友好的特性之一。


## `void`类型

在C语言中，有一个类型叫“void”，它用于两种用途：
1. 对于函数定义，“void”用来表示这个函数没有参数，或者没有返回值；
2. 对于指针，“void\*”表示这个指针可以指向任意类型。

> C语言的底层逻辑是，如果你对“void\*”解引用，得到的类型会是“void”，而“void”类型不能参与任何计算，这样就能巧妙地检测出一些代码错误。但是这个设计不能称之为优秀，因为很多C程序员其实不理解这个逻辑，他们只是记住了这条规则。

在E语言中，没有“void”这个类型。（编译器内部实现有，但是不会对外暴露，用户层面没有这个类型）

对于函数定义，我们不使用“void”，当我们的函数没有参数或者返回值，只要不写它们就好了。
对于指针，我们使用“any”这个类型，E语言中的“any^”等同于C语言中的“void\*”。

> 在C语言中，你不能直接省略参数，因为一些历史原因，C语言中省略参数和写上“void”，含义并不相同。


# 编译器

本编译器将E语言源码直接编译为RISC-V（32位rv32im）机器码。
后续可能会支持其他指令集/架构。

目前尚不支持命令行接口，需要从Erlang Shell里直接调用编译器进行编译：

使用实例：
```erlang
e_compiler:compile_to_machine1("./sample/led_sample_1.e", "/tmp/a.bin").
```


# 编辑器支持

本项目带有一个简单的Vim插件，在项目目录运行下面的命令可以进行安装：

```sh
mkdir -p ~/.vim/pack/my/start/
cp -r ./misc/elang.vim/ ~/.vim/pack/my/start/
```

