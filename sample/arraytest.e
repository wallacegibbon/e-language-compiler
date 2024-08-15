testglobal: u32 = 10;

fn main(argc: uptr, argv: i64^^): i64
	blah: {u8, 10};
	tmp: u8^ = blah@;
	tmp^ = 10;

	print(tmp);

	%myfn: fn (u8^) = inc;
	%myfn(tmp);

	myfn1: fn (): fn (): fn (u8^) = get_incfn1;
	myfn1()()(tmp);

	print(tmp);

	puts("before goto");
	goto finish;

	puts("after goto");

@@finish
	puts("after label");

	return (tmp + 11)^;
end

fn get_incfn1(): fn (): fn (u8^)
	return get_incfn2;
end

fn get_incfn2(): fn (u8^)
	return inc;
end

fn inc(val: u8^)
	cnt: u8 = 0;
	while cnt < 10 do
		(val + cnt)^ = cnt * 2;
		cnt += 1;
	end
end

puts: fn (i8^): i8;
printf: fn (i8^, usize);

fn print(val: u8^)
	cnt: u8 = 0;

	puts(">>>\t");
	while cnt < 10 do
		printf(" %02x", (val + cnt)^);
		cnt += 1;
	end
	cnt = puts("");
end

