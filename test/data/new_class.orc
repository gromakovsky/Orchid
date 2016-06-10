class A:
    public int64 i = 0
    public bool b = False

def mkA(i : int64, b : bool) → A *:
    new A res
    (*res).i = i
    (*res).b = b
    return res

def main():
    A * a1 = mkA(5, True)
    A * a2 = mkA(2, False)
    (*a1).i = 10
    stdWriteInt((*a1).i)
    stdWriteBool((*a1).b)
    delete a1
    stdWriteInt((*a2).i)
    stdWriteBool((*a2).b)
    delete a2
    return

