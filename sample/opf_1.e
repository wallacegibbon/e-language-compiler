struct MyI
	x			: word;
end

struct MyO
	i			: MyI;
end

fn app(o: MyI^): word
	return o^.x;
end

fn main()
	o1: MyO;
	o2: word;

	app(o1@);
	%app(o2@);
end
