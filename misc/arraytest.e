%% vim: ft=elang :

const TOTAL_NUM = 100;

fun main(argc: i64, argv: i64^^): i64
    blah: {i64, TOTAL_NUM};
    tmp: i64^ = blah@;
    tmp^ = TOTAL_NUM;
    myfn: fun(i64^): void = inc;
    myfn(tmp);
    return (tmp + 11)^;
end

fun inc(val: i64^): void
    cnt: u8 = 0;
    while (cnt < TOTAL_NUM)
	(val + cnt)^ = cnt * 2;
	cnt += 1;
    end
end
