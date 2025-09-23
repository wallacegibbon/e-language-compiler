printf: fn (byte^; word);

struct A
  tag: byte;
  value: word;
end

fn main(argc: word; argv: byte^^): word
  a: A = A{tag = 1, value = 12345678};
  printf("%x\n", (a.value@ as (byte^) + 2)^);
end
