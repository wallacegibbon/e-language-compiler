struct Blah
	id: byte^,
	a: Administrator^,
	%b: Administrator,
end

struct Blah1
	id: byte^,
	val: float,
	blob: word = sizeof(Blah),
end

struct User
	id: word = 10 * 20 + 3 * 4 + 22,
	%desc: {byte, 10} = {"hello"},
	desc: {byte, 10} = {1, 2, 3, 4, 5, 6, 7, 8, 9, 22},
	count: word,
	blah: Blah,
	b2: {Blah, 3},
	a: any^,
end

struct List
	next: List^,
	val: any^,
	ok: User = User{id = 1, count = 22},
	%invalid: List,
end

struct Administrator
	users: {User, 12},
	level: word,
end

% global variable
mod_info: {word, 100},
blah: word = 10,
blah1: word = sizeof(Blah1),

% t: {word, 3} = {0, 1, 2}, % t@^
% struct {word val[3],} t = {{0, 1, 2}}, // *(t.val)

% t: {User, 2} = {User{id = 1, desc = {"a"}}, User{id = 2, desc = {"b"}}},
% struct {User val[2],} t = {{1, 0, {"a"}}, {2, 0, {"b"}}},

u1: User = User{id = 8},

fn main(argc: word, argv: byte^^): word
	%invalid_u: User^ = User{id = 3}@,
	%users: {User, 22} = {User{nameref = 1}, User{id = 1}},
	%users: {User, 2} = {User{non = 1}, User{id = 1}},
	users: {User, 2} = {User{id = 1, blah = Blah{id = "a"}}, User{id = 1}},
	v0: byte = -1,
	users@^.blah = Blah{id = "b"},
	cnt: word = 22,
	init_users(users@, 22),

	%finish: byte,

	%goto a,

	v1: word = 1,
	v1 = v1 + 10,

	v1 as (Blah^)^.a,

	cnt = v1 + 2,
	cnt = 22,

	v2: byte = 2,
	v2 = v2 + 1,

	v3: word^ = v1@,

	f: fn (word^, byte^) = myfn,
	f(v1@, v2@),

	goto finish,

	sizeof(User),
	sizeof(Administrator),
	sizeof(Blah),
	sizeof(List),

	x: Blah1 = Blah1{id = "a"},
	y: Blah1 = Blah1{id = "b"},

	1 + x@,
	%1 - x@,

@@finish

	return 0,
end

fn init_users(users: User^, size: word)
	cnt: word = 30 + 52 * size / 2 + 100 / 10,
	while cnt < size do
		init_user(users + cnt, cnt, "test"),
	end
end

fn init_user(user: User^, id: word, desc: byte^)
	if id < 1 then
		user^.id = 1,
		user^.id = user^.id + 1,
	elif id == 5 then
		user^.id = 0,
	elif id == 10 then
		user^.id = 20,
	elif id == 20 then
		user^.id = 10,
	else
		user^.id = id,
	end
end

fn myfn(v1: word^, v2: byte^)
	v1^ += 1,
	v2^ += 2,
end

fn add(val: byte): byte
	return val * 3 + 1,
end

