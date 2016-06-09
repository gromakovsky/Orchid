def f() → int64 *:
    new int64 res
    return res

def main():
    int64 * i1 = f()
    int64 * i2 = f()
    *i1 = 1
    *i2 = 2
    stdWriteInt(*i1)
    delete i1
    stdWriteInt(*i2)
    delete i2
    return

