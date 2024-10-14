struct Blah
	id			: word;
	name			: byte^;
end

fn main()
	t1: Blah = Blah{id = 0, name = "test1"};
	t2: {word, 3} = {1, 2, 3};

	t1 = Blah{id = 8, name = "test2"};
	t2 = {11, 22, 33};

	ptest1(t1@);
	%ptest1(t2@);
	%ptest1(Blah{id = 16, name = "test3"}@);

	ptest2(t2@);
	%ptest2(t1@);
	%ptest2({1, 2, 3}@);
end

fn ptest1(p: Blah^): byte^
	return p^.name;
end

fn ptest2(a: word^): word
	return a[0];
end

