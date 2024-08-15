struct S1
	a: byte,
end

struct S2
	a: u16,
end

struct S3
	a: u64,
end

struct S4
	a: byte,
	b: u8,
	c: u16,
end

struct S5
	a: byte,
	b: u8,
	c: u32,
	%s: Sprite,
end

struct Sprite
	id: byte = 0,
	tag: u16 = 3,
	pos: Pos,
	tail: u8,
end

struct Pos
	x: u32 = 100,
	y: u32 = 101,
	h: u64 = 1000,
	s: S5,
end

g1: Sprite = Sprite{id = 1, pos = Pos{x = 2, y = 3}};
g2: u16 = 12;
g3: byte = 34;
g4: u32 = 56;
g9: Sprite = Sprite{id = 10, pos = Pos{x = 20, y = 30}};

fn main(): isize
	l1: Sprite = Sprite{id = 11, pos = Pos{x = 12, y = 13}};
	g1.pos.y = 111;
	l1.pos.y = 222;

	if
		l1.pos.y / 2 != 0
	then
		goto alignof_anchor;
	end

@@sizeof_anchor

	l1.pos.x = sizeof(Sprite);
	l1.pos.x = sizeof(Pos);
	l1.pos.x = sizeof(u32);

@@alignof_anchor

	l1.pos.x = alignof(Sprite);
	l1.pos.x = alignof(Pos);
	l1.pos.x = alignof(u32);

%@@alignof_anchor
	return 0;
end

fn blah(tag: byte, s: Sprite^): byte
	s^.pos.y = tag;
	return tag + 1;
end

fn ptest(s: any^)
	%s^.pos.y = 1;
	(s as Sprite^)^.pos.y = 1;
end

