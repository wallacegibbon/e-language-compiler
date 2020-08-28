%% vim: ft=elang :

const TOTAL_NUM = 10;

fun main(argc: i64, argv: i64^^): i64
    blah: {u8, TOTAL_NUM};
    tmp: u8^ = blah@;
    tmp^ = TOTAL_NUM;

    print(tmp);

    %myfn: fun(u8^) = inc;
    %myfn(tmp);

    myfn1: fun(): fun(): fun(u8^) = get_incfn1;
    myfn1()()(tmp);

    print(tmp);

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
    while(cnt < TOTAL_NUM)
	(val + cnt)^ = cnt * 2;
	cnt += 1;
    end
end

fun print(val: u8^)
    cnt: u8 = 0;

    c::printf(">>>");
    while(cnt < TOTAL_NUM)
	c::printf(" %d", (val + cnt)^);
	cnt += 1;
    end
    cnt = c::printf(".\n");
end

