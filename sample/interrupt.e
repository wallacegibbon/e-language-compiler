interrupt_vector_table: {word, 3} = {fn2 as (word), 0, fn3 as (word)};

flag: word = 0;

fn fn1()
	v: word = 1;
	flag += v;
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
	return a + b + c;
end

fn main()
	fn3(1, 2);
end
