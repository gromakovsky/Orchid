def f(a : int64*):
    stdWriteInt(*a)
    return

def main():
    int64 a = 10
    f(&a)
    return

