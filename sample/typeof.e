struct A
	a: byte^,
	x: typeof(instance_b),
end

struct B
	blah: typeof(global_tag) = "a",
	x: typeof(instance_a@),
end

instance_a: A,
instance_b: B,
global_tag: byte^,

fn blah(): typeof(blah2)
	b: B = B{blah = "hello"},
	s: typeof(b),
	s.blah = "world",
end

fn blah2(): byte
	return 1,
end
