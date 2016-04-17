def power():
    int32 k = readInt32()
    int32 n = readInt32()
    int32 r = 1
    while k > 0:
        if k % 2 == 1:
            r = r * n
        else:
            pass

        n = n * n
        k = k / 2

    writeInt32(r)

