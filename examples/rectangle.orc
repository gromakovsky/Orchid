class Point:
    public int64 x = 0
    public int64 y = 0


def abs(x : int64) → int64:
    if x < 0:
        return -x
    else:
        return x


class Rectangle:

    public Point p0 = Point()
    public Point p1 = Point()

    public def printSquare():
        int64 dx = abs(p0.x - p1.x)
        int64 dy = abs(p0.y - p1.y)
        stdWriteInt(dx * dy)
        return

    public def printP1X():
        stdWriteInt(p1.x)
        return


def main():
    Point p1 = Point()
    p1.x = 1
    p1.y = 2
    Rectangle rect = Rectangle()
    rect.p1 = p1
    rect.printSquare()
    rect.printP1X()
    return

