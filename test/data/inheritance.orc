class A:
    public def f():
        stdWriteInt(1)
        return

class B(A):
    public def ff():
        f()
        return

def main():
    B b = B()
    b.ff()
    return

