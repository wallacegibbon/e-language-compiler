---
title: “E编程语言”
---

[English version](README.md)

# 介绍

“E语言”是一个简化版的“C语言”，它有着更少的概念，更合理的语法，它的设计理念是：


- 指针操作更“显式” `E` xplicit
- 容易使用和实现 `E` asy
- 专注嵌入式系统 `E` mbedded systems，适合电子爱好者 `E` lectronic hobbyists


以下是C语言和E语言在基本操作上的一些对比：

## 基本操作

| C语言                   | E语言            |
|-------------------------|------------------|
| &p                      | p@               |
| *p                      | p^               |
| ***p                    | p^^^             |
| p[3]                    | p[3]             |
| &p[3]                   | p[3]@            |
| p + 3                   | p[3]@            |
| (char *)p + 1           | p + 1            |
| p.m                     | p.m              |
| (*p).m                  | p^.m             |
| p-\>m                   | p^.m             |
| char *p                 | p: byte^         |
| void **p                | p: any^^         |
| ((struct Blah *) p)->f1 | p as (Blah^)^.f1 |

> 在C语言中，数组访问和指针操作完全等价，即 `p[2]` 完全等价于 `*(p + 2 * sizeof(*p))` ，
> 语义上是一种浪费。
> 在E语言中， `p + 2` 不像C语言中表示 `p + 2 * N` ，
> 而是单纯表达 `p + 2` ，而 `p[2]` 则等同于C语言中的 `p[2]` ，
> 不用再犹豫何时使用数组操作何时使用指针操作了。


## 数组和结构体

E语言：
```elang
arr: {word, 3} = {1, 2, 3};

struct Blah
    id: word;
    name: byte^;
end

b: Blah = Blah{id = 1, name = "hello"};

c : {Blah, 2} = {Blah{id = 1, name = "a"}, Blah{id = 2, name = "b"}};
```

C语言：
```c
int32_t arr[3] = {1, 2, 3};

struct Blah {
    int id;
    char *name;
};

struct Blah b = {1, "hello"};
struct Blah c[2] = {{1, "a"}, {2, "b"}};
```


## 联合体

C语言中“联合体”主要的用途，是复用内存，这个在特定使用场景下比较方便，但是同时，
联合体操作可以简单被类型转换加指针操作替换，举个例子：


```c
struct A {
    char tag;
    union {
        ong long num;
        har buf[8];
    }
    value;
};

struct A a = {.tag = 1, .value.num = 0x12345678 };
printf("%x\n", a.value.buf[2]);
//> 34
```

在E语言中（或者C语言中），我们可以通过下列指针操作取代联合体：

```elang
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

枚举是很好的概念，它带来了更强的类型检测，但同时，没有枚举并不会带来实质性的功能缺失。
为了保证语言更加简单，E语言也放弃了对枚举的支持。


## 函数定义

```elang
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

```elang
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

```elang
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

```elang
my_fn1: fn(): fn(): fn() = another_fn1;
my_fn2: fn(byte^; word): fn(byte^; byte^): fn(word; word): byte^ = another_fn2;
```

```c
void (*(*(*my_fn1)())())() = another_fn1;
char *(*(*(*my_fn2)(char *, int))(char *, char *))(int, int) = another_fn2;
```


## 中断

对于嵌入式系统，中断处理非常重要和基础，在E语言里，要定义一个ISR，只需要直接写：

```elang
fn exti_isr() attribute(interrupt(26))
    %% Clear interrupt flag.
    exti4^.INTF = 0b10000;
    %...
end
```

这里的 `26` 表示的是中断的ID，这个ID可以直接在芯片手册中查找到。

> 用C语言的时候，用户往往需要写汇编代码和链接脚本来配合ISR，比较麻烦。
> E语言省去了这层麻烦。只需要芯片手册，就能轻松编写中断相关的代码，这是E语言对电子爱好者友好的特性之一。

## `void` 类型

在C语言中，有一个类型叫“void”，它用于两种用途：
1. 对于函数定义，“void”用来表示这个函数没有参数，或者没有返回值；
2. 对于指针，“void*”表示这个指针可以指向任意类型。

> C语言的底层逻辑是，如果我们对“void*”解引用，得到的类型会是“void”，而“void”类型不
> 能参与任何计算，这样就能巧妙地检测出一些代码错误。但是这个设计不能称之为优秀，因
> 为很多C程序员其实不理解这个逻辑，他们只是记住了这条规则。

在E语言中，没有“void”这个类型。（编译器内部实现有，但是不会对外暴露，用户层面没有这个类型）

对于指针，我们使用“any”这个类型，E语言中的“any^”等同于C语言中的“void*”。

对于函数定义，我们不使用“void”。当我们的函数没有参数或者返回值，只要不写它们就好了。

> 在C语言中，我们不能直接省略参数，因为一些历史原因，C语言中省略参数和写上“void”，含义并不相同。

## 布尔表达式

在C语言中，任何表达式都是布尔表达式，这个特性造成了非常非常多的错误代码。

人们很容易写出下面这样的错误代码：
```c
if (a = b) {
    //...
}
```

在语言中，只有6中布尔表达式： `>`, `>=`, `<`, `<=`, `!=`, `==`.

所以下面的代码，会被编译器拒绝：
```elang
if a = b then
    %...
end
```

编译报错：
```
./sample/led_sample_1.e:115:9: invalid boolean expression for if
```


## 位操作

由于我们将“^”用于指针操作符了（“^”看起来就像一个“指针”），所以无法保持跟C语言相同的位操作。（C语言将“^”用于异或操作）

作为替代，E语言选择了Erlang风格的位操作。

E语言定义了关键字 “band”，“bor”，“bnot”，“bxor”来进行位逻辑运算，它们分别对应C语言中的“&”，“|”，“`”，“^”。

同时，定义了“bsl”，“bsr”作为移位操作符，它们对应到C语言中的“<<”，“>>”。（“bsl”是“bit shift left”的首字母缩写）

位操作很重要，但是它们没有指针操作常用，这是E语言把“^”留给指针操作的主要原因。


## 宏

E语言支持类似C语言的词法级别的“宏”。定义的时候，基本和C语言用法一样：

```elang
#define GPIOD (0x4001_1400 as (GPIO^))
```

带参数的宏目前不支持。

使用宏的时候，和C语言不同，我们需要在宏名字前面加一个 `?` 符号。（这是参照Erlang中宏的用法设计的）

```elang
?GPIOD^.BSH = 0b1_1101;
```

> 在C语言中，我们很难知道一个符号是宏，还是变量，还是函数，这个特性有时会让代码变得难以理解。
> 通过引入 `?` ，我们一眼就能看出哪里是宏。


# 编译器

本编译器将E语言源码直接编译为RISC-V（32位RV32I/RV32E）机器码。

要使用E语言编译器，我们可以用命令行工具 `ec` 。

CH32V307示例：
```sh
ec -i ./sample/ch32v.e ./sample/led_sample_1.e -o /tmp/a \
--v-pos 0 --v-size 416 \
--c-pos 416 \
--d-pos 0x2000_0000 --d-size 64K \
--v-init-jump
```

CH32V003示例：
```sh
ec -i ./sample/ch32v.e ./sample/led_sample_2.e -o /tmp/a \
--v-pos 0 --v-size 156 \
--c-pos 156 \
--d-pos 0x2000_0000 --d-size 2K \
--v-init-jump --prefer-shift
```

我们会得到两个bin文件： `a.code.bin` （代码）和 `a.ivec.bin` （中断向量表）。
用烧写工具将这两个bin烧录到对应的地址即可。

如果你是一名Erlang用户，你也可以从Erlang Shell里直接调用编译器进行编译：
```erlang
e_compiler:compile_to_machine1(["./sample/ch32v.e", "./sample/led_sample_1.e"],
			       "/tmp/a", #{...}).
```

要编译编译器，可以查看[Build.cn.md](./BUILD.cn.md)。


# 编辑器支持

## Vim

Vim:

```sh
mkdir -p ~/.vim/pack/wallacegibbon/start/
cp -r ./misc/vim ~/.vim/pack/wallacegibbon/start/elang
```

NeoVim:

```sh
mkdir -p ~/.local/share/nvim/site/pack/wallacegibbon/start
cp -r ./misc/vim ~/.local/share/nvim/site/pack/wallacegibbon/start/elang
```

## Emacs

```sh
mkdir -p ~/.emacs.d/misc/
cp ./misc/emacs/elang-mode.el ~/.emacs.d/misc/
```

然后在“~/.emacs”里面，加入下面的内容：
```emacslisp
(add-to-list 'load-path "~/.emacs.d/misc/")
(require 'elang-mode)
```
