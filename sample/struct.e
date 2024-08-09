struct Sprite
	id: byte,
	pos: Pos,
end

struct Pos
	x: u32,
	y: u32,
end

s1: Sprite = Sprite{id = 1, pos = Pos{x = 2, y = 3}};

fun main()
	s2: Sprite = Sprite{id = 11, pos = Pos{x = 12, y = 13}};
	s1.pos.y = 111;
	s2.pos.y = 222;
end

