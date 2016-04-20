def power():
    int64 k = stdReadInt()
    int64 n = stdReadInt()
    int64 r = 1
    while k > 0:
        if k % 2 == 1:
            r = r * n

        n = n * n
        k = k / 2

    stdWriteInt(r)

