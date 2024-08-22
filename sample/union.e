printf: fn (byte^, usize),

struct A
	tag: u8,
	value: i64,
end

fn main(argc: isize, argv: byte^^): i64
	a: A = A{tag = 1, value = 12345678},
	printf("%x\n", (a.value@ as (byte^) + 2)^),
end
