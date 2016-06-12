class A:
    public int64 x = 10

    public def f():
        stdWriteInt(x)
        return

class B(A):
    public int64 y = 15

    public def ff():
        f()
        stdWriteInt(x + y)
        return

def main():
    B b = B()
    b.ff()
    return

