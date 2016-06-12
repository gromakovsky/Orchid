class A:

    public virtual def f() → int64:
        return 1


class B(A):

    public virtual def f() → int64:
        return 2

    public virtual def g() → int64:
        return 22


class C(B):

    public virtual def g() → int64:
        return 23


def printF(a : A *):
    stdWriteInt((*a).f())
    return


def printG(b : B *):
    stdWriteInt((*b).g())
    return


def main():
    A a = A()
    B b = B()
    C c = C()
    printF(&a)
    printF(&b)
    printF(&c)
    printG(&b)
    printG(&c)
    return

