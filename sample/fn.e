fn fn0(a: word; b: word)
	tmp: word = 11;
	tmp = a + b * 2 + tmp;
end

fn fn1(): fn(word; word)
	return fn0;
end

fn fn2(): fn(): fn(word; word)
	return fn1;
end

fn main(v: word)
	f: fn(): fn(): fn(word; word)
	f = fn2;
	return f()()(v + 1, v + 2);
end


