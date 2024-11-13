struct MyI
	x				: word;
end

struct MyO
	i				: MyI;
end

fn app(o: MyI^): word
	return o^.x;
end

fn main()
	o1: MyO;
	o2: word;
	i1: MyI;

	po1: MyO^;
	pi1: MyI^;

	%i1 = o1; %ERROR
	%o1 = i1;
	pi1 = o1@;
	po1 = i1@;

	app(o1@);
	%app(o2@);
end
