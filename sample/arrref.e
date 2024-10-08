fn main()
	a: {word, 3};
	a@[2] = 11;
	(a@ + 2 * sizeof(word))^ = 22;
end
