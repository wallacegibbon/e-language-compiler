printf: fn (i8^, usize);

struct A
	tag: u8,
	value: i64,
end

fn main(argc: isize, argv: i64^^): i64
	a: A = A{tag = 1, value = 0x12345678};
	printf("%x\n", (a.value@ as (i8^) + 2)^);
end
