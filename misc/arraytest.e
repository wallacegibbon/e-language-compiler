%% vim: ft=elang :

const TOTAL_NUM = 10;

fun main(argc: i64, argv: i64^^): i64
    blah: {u8, TOTAL_NUM};
    tmp: u8^ = blah@;
    tmp^ = TOTAL_NUM;

    print(tmp);

    myfn: fun(u8^): void = inc;
    myfn(tmp);

    print(tmp);

    return (tmp + 11)^;
end

fun inc(val: u8^): void
    cnt: u8 = 0;
    while (cnt < TOTAL_NUM)
	(val + cnt)^ = cnt * 2;
	cnt += 1;
    end
end

fun print(val: u8^): void
    cnt: u8 = 0;

    c::printf(">>>");
    while (cnt < TOTAL_NUM)
	c::printf(" %d", (val + cnt)^);
	cnt += 1;
    end
    c::printf(".\n");
end

