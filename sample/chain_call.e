fn fn0(a: word; b: word): word
	tmp: word = 11;
	return a + b * 2 + tmp;
end

fn fn1(): fn(word; word): word
	return fn0;
end

fn fn2(): fn(): fn(word; word): word
	return fn1;
end

fn main()
	v: word;
	v = 9 + fn2()()(v + 1, v + 2) + 8;
end

