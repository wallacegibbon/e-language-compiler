fn main()
	tmp: word;
	n: word = 10;

	tmp = tmp bsl 0;
	tmp = tmp bsl 10;
	tmp = tmp bsl 20;
	tmp = tmp bsl 30;
	tmp = tmp bsl 32;
	%tmp = tmp bsl 33;
	%tmp = tmp bsr 33;
	tmp = tmp bsr 32;

	tmp = tmp bsl n;
	tmp = tmp bsr n;
end
