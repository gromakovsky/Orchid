def fib(n : int64) → int64:
    if n < 0:
        stdExit(100)
        return -1
    else:
        if n == 0 or n == 1:
            return 1
        else:
            return fib(n - 2) + fib(n - 1)

def main():
    stdWriteInt(fib(5))
    return

