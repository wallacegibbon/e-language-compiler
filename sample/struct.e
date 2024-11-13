#include "blah.h"

#define PRINT_STRING "hello, world !\n"
#define RELEASE 1

struct S1
	a				: byte;
end

struct S2
	a				: byte;
end

struct S3
	a				: word;
end

struct S4
	a				: byte;
	b				: byte;
	c				: byte;
end

struct S5
	a				: byte;
	b				: byte;
	c				: word;
	%s				: Sprite;
end

struct Sprite
	id				: byte = 0;
	tag				: byte = 3;
	pos				: Pos;
	tail				: byte;
end

struct Pos
	%x				: word = "hello";
	x				: word = 100;
	y				: word = 101;
	h				: word = 1000;
	s				: S5;
end

g1: Sprite = Sprite{id = 1, pos = Pos{x = 2, y = 3}};
g2: byte = 12;
g3: byte = 34;
g4: word = 56;
g9: Sprite = Sprite{id = 10, pos = Pos{x = 20, y = 30}};

fn main(): word
	l1: Sprite = Sprite{id = 11, pos = Pos{x = 12, y = 13}};
	g1.pos.y = 111;
	%l1.pos.y = "hello";
	l1.pos.y = 222;

	if l1.pos.y / 2 != 0 then
		goto alignof_anchor;
	end

	l1.pos.y = g1.pos.y;

@@sizeof_anchor

	l1.pos.x = sizeof(Sprite);
	l1.pos.x = sizeof(Pos);
	l1.pos.x = sizeof(word);

@@alignof_anchor

	l1.pos.x = alignof(Sprite);
	l1.pos.x = alignof(Pos);
	l1.pos.h = alignof(Pos) + 1;
	l1.pos.h = 1 + (1 + (1 + (alignof(Pos) + 1)));

#if ?RELEASE
	l1.pos.x = alignof(word);
#else
	l1.pos.x = alignof(byte);
#endif

%@@alignof_anchor
	return 0;
end

fn blah(tag: byte; s: Sprite^): byte
	str: byte^ = ?PRINT_STRING;
	s^.pos.y = tag;
	return tag + 1;
end

fn ptest(s: any^; cnt: word)
	%s^.pos.y = 1;
	s as (Sprite^)^.pos.y = 'a';
	fn2(s as (byte^) + cnt);
	fn1(fn2(s@ as (byte^) + cnt));
	((fn1 as (byte^)^@^@ + 2 + 3) as (fn(byte^): byte^))(s as (byte^));
end

fn fn1(arg: byte^): byte^
	tmp: byte^;
	1 + arg;
	arg - 1;
	%1 - arg;
	a: word;
	a = arg - tmp;
	return arg + 1;
end

fn fn2(arg: byte^): byte^
	v1: byte;
	v2: byte;
	v2 = v1 * arg as (byte^)^;
	return arg + 2;
end

