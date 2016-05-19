class Point:
    public int64 x = 10
    public int64 y = 15

    public def printSum(a : int64):
        stdWriteInt(x + y + a)
        return


def main():
    Point p = Point()
    p.x = p.x + 10
    p.y = p.y - 10
    p.printSum(17)
    return

