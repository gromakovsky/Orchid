class Shape:

    public virtual def square() → int64:
        stdExit(19)
        return 0


class Point(Shape):

    private int64 x = 0
    private int64 y = 0

    public virtual def square() → int64:
        return 0

    public def getX() → int64:
        return x

    public def setX(xx : int64):
        x = xx
        return

    public def getY() → int64:
        return y

    public def setY(yy : int64):
        y = yy
        return


class Circle(Shape):

    private Point p = Point()
    private int64 radius = 1

    public virtual def square() → int64:
        return 3 * radius * radius

    public def setRadius(r : int64):
        radius = r
        return


def abs(x : int64) → int64:
    if x < 0:
        return -x
    else:
        return x


class Rectangle(Shape):

    private Point p0 = Point()
    private Point p1 = Point()

    public virtual def square() → int64:
        int64 dx = abs(p0.getX() - p1.getX())
        int64 dy = abs(p0.getY() - p1.getY())
        return dx * dy

    public def setP1(p : Point):
        p1 = p
        return


def printSquare(shape : Shape *):
    stdWriteInt((*shape).square())
    return


def main():
    Point p1 = Point()
    p1.setX(3)
    p1.setY(-1)
    Circle c = Circle()
    c.setRadius(2)
    Rectangle rect = Rectangle()
    rect.setP1(p1)
    printSquare(&p1)
    printSquare(&c)
    printSquare(&rect)
    return

