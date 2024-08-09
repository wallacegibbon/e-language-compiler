struct Sprite
	id: byte,
	pos: Pos,
end

struct Pos
	x: u32,
	y: u32,
end

fun blah()
	sprite: Sprite = Sprite{id = 11, pos = Pos{x = 100, y = 200}};
	sprite.id = 12;
	sprite@;
end

