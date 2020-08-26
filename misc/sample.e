%% vim: ft=elang :

struct Blah
    id: i8^,
end

struct User
    id: i64 = 10 * 20 + 3 * 4 + TOTOAL_USERCNT,
    %desc: {i64, 10} = {"hello"},
    desc: {i64, 10} = {1,2,3,4,5,6,7,8,9,TOTOAL_USERCNT},
    count: u32,
    blah: Blah,
    b2: {Blah,3},
end

struct List
    next: List^,
    val: void^,
    ok: User = User{id=1, count=TOTOAL_USERCNT},
    %invalid: List,
end

struct Administrator
    users: {User, TOTAL_ADMIN_LIMIT + 1},
    level: i64,
end

% global variable
mod_info: {i64, 100};
blah: i64 = 10;

const TOTAL_ADMIN_LIMIT = 10 + 2;

% t: {i64, 3} = {0, 1, 2}; % t@^
% struct {i64 val[8];} t = {{0, 1, 2}}; // *(t.val)

% t: {User, 2} = {User{id=1, desc={"a"}}, User{id=2, desc={"b"}}};
% struct {User val[2];} t = {{1, 0, {"a"}}, {2, 0, {"b"}}};

const BASE_MUL = 12;
const TOTOAL_USERCNT = 10 + 3 * BASE_MUL - 1;
const blah1 = 1 bsl 8;
const blah2 = 1 bsr 8;

fun main(argc: i64, argv: i64^^): i64
    %users: {User, TOTOAL_USERCNT} = {User{nameref=1}, User{id=1}};
    %users: {User, 2} = {User{non=1}, User{id=1}};
    users: {User, 2} = {User{id=0, blah=Blah{id="a"}}, User{id=1}};
    v0: i8 = -1;
    users@^.blah = Blah{id="b"};
    cnt: i64 = TOTOAL_USERCNT;
    %init_users(users@, sizeof(users)/sizeof(User));
    init_users(users@, TOTOAL_USERCNT);

    v1: i64 = 1;
    v1 = v1 + 10;

    cnt = v1 + 2;
    cnt = TOTOAL_USERCNT;

    v2: u8 = 2;
    v2 = v2 + 1;

    v3: i64^ = v1@;

    f: fun(i64^): void = myfn;
    f(v1@);

    %mym::f();
    c::malloc(30);

    return 0;
end

fun init_users(users: User^, size: i64): void
    cnt: i64 = 30 + 52 * size / 2 + 100 / 10;
%    while (cnt < size)
%	init_user((users + cnt)^, cnt, "test");
%    end
end

fun init_user(user: User^, id: i64, desc: i64^): void
    if (id < 1)
	user^.id = 1;
	user^.id = user^.id + 1;
    elif (id == 5)
	user^.id = 0;
    elif (id == 10)
	user^.id = 20;
    elif (id == 20)
	user^.id = 10;
    else
	user^.id = id;
    end
end

fun myfn(any: i64^): void
    any^ += 1;
end
