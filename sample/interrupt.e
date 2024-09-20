flag: word = 0,

fn fn1()
	v: word = 1,
	flag += v,
end

interrupt
fn fn2()
	v: word = 2,
	flag += v,
end

