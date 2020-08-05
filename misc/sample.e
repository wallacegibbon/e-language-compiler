%% vim: ft=erlang :

struct User
    id: u32,
    description: block<i8, 10>,
end

% t: block<i8, 10>,   t@ --> i8^

const TOTOAL_USERCNT = 10

main(argc: isize, argv: i8^^): isize ->
    users: block<User, TOTOAL_USERCNT> = alloc(size<User> * TOTOAL_USERCNT),
    init_users(users@, users.len).

init_users(users: User^, size: u8): void ->
    init_users(users, 0, size).

init_users(users: User^, cnt: u8, size: u8): void when cnt < size ->
    u: User = (users + cnt)^,
    u.id = cnt,
    memcpy(u.description@, "hello"),
    init_users(users, cnt + 1);
init_users(users: User^, cnt: u8, size: u8): void ->
    pass.

