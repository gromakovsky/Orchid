class Point:
    public int64 x = 10
    public int64 y = 15

    public def printSum(a : int64):
        stdWriteInt(x + y + a)
        return

Point p = Point()

def main():
    p.printSum(17)
    return

