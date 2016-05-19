class A:
    public def f():
        stdWriteInt(1)
        return

    public def ff():
        stdWriteInt(2)
        f()
        return

def main():
    A a = A()
    a.ff()
    return

