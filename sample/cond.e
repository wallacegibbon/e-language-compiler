struct MyObj
	name: byte^;
	id: word;
end

fn cond_test1(): word
	val1: byte = 10;
	val2: word = 1000;
	result: word;

	if not(val1 / 2 != 0 and val2 / 2 != 0) or val1 + val2 == 0 then
		result = val1 + val2;
	else
		result = val1 / val2;
	end

	return result;
end

fn str_compare(dest: byte^; src: byte^): byte
	while src^ != 0 do
		dest^ = src^;
		dest += 1;
		src += 1;
	end

	if src^ != 0 then
		return 1;
	else
		return 0;
	end
end

fn print_str(fmt: byte^; n: word; data: byte^^): word
	return 0;
end

fn cond_test2(self: MyObj^; prefix: byte^; postfix: byte^): word
	args: {byte^, 1} = {self^.name};
	if str_compare(self^.name, "wallace") == 0 then
		print_str("hello, %s\n", 1, args@);
	end
	return 0;
end

