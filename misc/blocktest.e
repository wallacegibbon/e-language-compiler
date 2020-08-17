fun main(argc: i64, argv: i64^^): i64
    blah: <i64, 10>;
    tmp: i64^ = blah@;
    tmp^ = 100;
    return tmp^;
end
