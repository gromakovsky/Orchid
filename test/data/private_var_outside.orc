class Point:
    private int64 x = 10
    private int64 y = 15

    public def printSum(a : int64):
        stdWriteInt(x + y + a)
        return


def main():
    Point p = Point()
    p.x = 10
    p.printSum(17)
    return

