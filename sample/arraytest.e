%% vim: ft=elang :

testglobal: u32 = 10;

fun main(argc: isize, argv: i64^^): i64
    blah: {u8, 10};
    tmp: u8^ = blah@;
    tmp^ = 10;

    print(tmp);

    %myfn: fun(u8^) = inc;
    %myfn(tmp);

    myfn1: fun(): fun(): fun(u8^) = get_incfn1;
    myfn1()()(tmp);

    print(tmp);

    puts("before goto");
    goto finish;

    puts("after goto");

@@finish:
    puts("after label");

    return (tmp + 11)^;
end

fun get_incfn1(): fun(): fun(u8^)
    return get_incfn2;
end

fun get_incfn2(): fun(u8^)
    return inc;
end

fun inc(val: u8^)
    cnt: u8 = 0;
    while cnt < 10 do
        (val + cnt)^ = cnt * 2;
        cnt += 1;
    end
end

puts: fun (i8^): i8;
printf: fun (i8^, usize);

fun print(val: u8^)
    cnt: u8 = 0;

    puts(">>>\t");
    while cnt < 10 do
        printf(" %02x", (val + cnt)^);
        cnt += 1;
    end
    cnt = puts("");
end

