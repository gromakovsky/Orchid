def stdPower(a: int64, b: int64) → int64:
    if a == 0 and b <= 0:
       stdExit(3)

    if b < 0:
        a = 1 / a
        b = -b

    int64 r = 1
    while b > 0:
        if b % 2 == 1:
            r = r * a

        a = a * a
        b = b / 2

    return r
