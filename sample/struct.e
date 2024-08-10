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
end

struct Sprite
	id: byte = 0,
	pos: Pos,
end

struct Pos
	x: u32 = 100,
	y: u32 = 101,
	h: u64 = 1000,
end

s1: Sprite = Sprite{id = 1, pos = Pos{x = 2, y = 3}};

fun main()
	s2: Sprite = Sprite{id = 11, pos = Pos{x = 12, y = 13}};
	s1.pos.y = 111;
	s2.pos.y = 222;
end

