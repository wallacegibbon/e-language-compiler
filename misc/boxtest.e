%% vim: ft=elang :

fun main(argc: i64, argv: i64^^): i64
    blah: {i64, 10};
    tmp: i64^ = blah@;
    tmp^ = 100;
    myfn: fun(i64^): void = inc;
    myfn(tmp);
    return tmp^;
end

fun inc(val: i64^): void
    val^ = val^ + 2;
end
