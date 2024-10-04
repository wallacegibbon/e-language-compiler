fn fn0(a: word; b: word): void
	tmp: word = 11;
	tmp = a + b * 2 + tmp;
end

fn fn1(): fn(word; word): void
	return fn0;
end

fn fn2(): fn(): fn(word; word): void
	return fn1;
end

fn main(v: word): void
	%% Both are ok
	f: fn(): fn(): fn(word; word): void;
	%f: fn(): fn(): fn(word; word);

	f = fn2;
	return f()()(v + 1, v + 2);
end


