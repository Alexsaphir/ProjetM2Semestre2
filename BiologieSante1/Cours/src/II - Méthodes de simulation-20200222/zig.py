from openturns import *

abscisses = Sample.ImportFromCSVFile("zigAbs.txt")
print("abs=")
print(abscisses)
dist = Normal()
g = dist.drawPDF(0,6,101)
g.setTitle("Ziggurat method")
for i in range(abscisses.getSize() - 1):
    x1 = abscisses[i][0]
    y1 = dist.computePDF(x1)
    x2 = abscisses[i+1][0]
    y2 = dist.computePDF(x2)
    data = Sample(5, 2)
    data[0][0] = 0
    data[0][1] = y1
    data[1][0] = x2
    data[1][1] = y1
    data[2][0] = x2
    data[2][1] = y2
    data[3][0] = 0
    data[3][1] = y2
    data[4][0] = 0
    data[4][1] = y1
    c = Curve(data)
    g.add(c)
    if i > 0:
        data = Sample(2, 2)
        data[0][0] = x1
        data[0][1] = y1
        data[1][0] = x1
        data[1][1] = y2
        c = Curve(data)
        c.setColor("magenta")
        g.add(c)
data = Sample(3, 2)
x = abscisses[abscisses.getSize() - 1][0]
y = dist.computePDF(x)
data[0][0] = 0
data[0][1] = y
data[1][0] = 0
data[1][1] = 0
data[2][0] = 6
data[2][1] = 0
c = Curve(data)
g.add(c)
data = Sample(2, 2)
data[0][0] = x
data[0][1] = y
data[1][0] = x
data[1][1] = 0
c = Curve(data)
c.setColor("magenta")
g.add(c)
g.draw("zigBigMC")
