%% vim: ft=erlang :

defbox User as
    id: U32,
    description: <I8, 10>.

% t: block<i8, 10>,   t@ --> i8^

-define(TOTOAL_USERCNT, 10).

main(argc: I32, argv: I8^^): I32 ->
    users: <User, ?TOTOAL_USERCNT>,
    init_users(users@, users.len).

init_users(users: User^, size: U8): Any ->
    init_users(users, 0, size).

init_users(users: User^, cnt: U8, size: U8): Any when cnt < size ->
    u: User = (users + cnt)^,
    u.id = cnt,
    memcpy(u.description@, "hello"),
    init_users(users, cnt + 1);
init_users(users: User^, cnt: U8, size: U8): Any ->
    0.

