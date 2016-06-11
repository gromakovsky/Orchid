def f(i : int64) → int64:
    if i < 3:
        return 1
    else:
        return 2 + g(i - 1)

def g(i : int64) → int64:
    if i <= 1:
        return 3
    else:
        return 5 * f(i - 2)

def main():
    stdWriteInt(f(7))
    return

