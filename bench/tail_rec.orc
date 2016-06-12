def f(n : int64) → int64:
    if n < 1:
        return 1
    else:
        return n * f(n - 1)

def main():
    int64 n = 80000
    int64 iter = 300000
    while iter > 0:
        stdWriteInt(f(10000))
        iter = iter - 1
    return

