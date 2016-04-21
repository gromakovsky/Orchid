def main() → int64:
    int64 n = stdReadInt()
    if n < 0:
        stdExit(100)

    int64 r = 1
    while n > 1:
        r = r * n
        n = n - 1

    stdWriteInt(r)
    return 0
