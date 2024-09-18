fn fn0(v: word): word
	return v + 1,
end

fn fn1(): fn(word): word
	return fn0,
end

fn fn2(): fn(): fn(word): word
	return fn1,
end

fn fn3(v: word): word
	return 2 + fn2()()(v + 1) + 3,
end

