flag: word = 0;

fn fn1()
	v: word = 1;
	flag += v;
	flag += 3;
	flag += 2047;
	flag += 2048;
	flag -= 2048;
	flag -= 2049;
	flag -= v;
end

interrupt
fn fn2()
	v: word = 2;
	flag += v;
end

fn fn3(
	a: word;
	b: word;
): word
	c: word = 3;
	if c bsl 3 != 0 then
		a += 1;
	end
	return a + b + 5 + c - 2048;
end

fn main()
	fn3(1, 2);
end

