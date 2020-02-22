from openturns import *
from time import sleep
from math import *

def inDone(k, done):
    for i in range(done.getSize()):
        if fabs(k - done[i]) < 1e-10:
            return True
    return False

draw = True
PRNG = "Random"
#color = "red"
#index1 = 0
#index2 = 10
#color = "green"
#index1 = 0
#index2 = 30
#color = "black"
#index1 = 0
#index2 = 53
#color = "magenta"
#index1 = 0
#index2 = 103
data = NumericalSample.ImportFromCSVFile("data" + PRNG + ".csv")
pMin = data.getMin()
pMax = data.getMax()
a = max(pMax[0], pMax[1])
if draw:
    g = Graph("Corner test", "x", "y", True, "")
    c = Cloud(data)
    c.setPointStyle("bullet")
    g.addDrawable(c)
    g.draw("Corner" + PRNG, 1300, 1200)
    Show(g)
index1 = 0
p1 = data[index1]
goodColors = Description(["red", "green", "magenta", "black", "darkcyan", "purple"])
iColor = 0
okLattice = NumericalPoint(0)
slopes = NumericalPoint(0)
for index2 in range(1, data.getSize()):
    p2 = data[index2]
    dx = p2[0] - p1[0]
    dy = p2[1] - p1[1]
    if dx == 0.0:
        dx += 1e-15
    if dy == 0.0:
        dy += 1e-15
    s = dy / dx
    stop = True
    if not inDone(s, slopes):
        slopes.add(s)
        done = NumericalPoint(0)
        if draw:
            lines = DrawableCollection(0)
        for i in range(data.getSize()):
            xp = data[i][0]
            yp = data[i][1]
            k = yp * dx - xp * dy
            flag = inDone(k, done)
            if not flag:
                done.add(k)
                stop = done.getSize() > 50
                if draw:
                    # point (0, y0)
                    y0 = yp + dy * (0.0 - xp) / dx
                    # point (x0, 0)
                    x0 = xp + dx * (0.0 - yp) / dy
                    # point (a, ya)
                    ya = yp + dy * (a - xp) / dx
                    # point (xa, a)
                    xa = xp + dx * (a - yp) / dy
                    # Which line to draw?
                    #
                    #  1 | 2 | 3
                    # ---+---+---
                    #  4 | 5 | 6
                    # ---+---+---
                    #  7 | 8 | 9
                    #
                    # The left-most point can be in:
                    # 2: (xa, a)
                    # 5: (0, y0)
                    # 8: (x0, 0)
                    # The right-most point can be in:
                    # 2: (xa, a)
                    # 5: (a, ya)
                    # 8: (x0, 0)
                    # Left-most point
                    if y0 >= 0.0:
                        if y0 <= a:
                            # Region 5
                            pLeft = NumericalPoint([0, y0])
                        else:
                            # Region 2
                            pLeft = NumericalPoint([xa, a])
                    else:
                        # Region 8
                        pLeft = NumericalPoint([x0, 0])
                    # Right-most point
                    if ya >= 0.0:
                        if ya <= a:
                            # Region 5
                            pRight = NumericalPoint([a, ya])
                        else:
                            # Region 2
                            pRight = NumericalPoint([xa, a])
                    else:
                        # Region 
                        pRight = NumericalPoint([x0, 0])
                    dataLine = NumericalSample(0, 2)
                    dataLine.add(pLeft)
                    dataLine.add(pRight)
                    line = Curve(dataLine)
                    line.setColor("red")
                    lines.add(line)
            if stop:
                break
        if not stop:
            print("index2=", index2, "good, size=", done.getSize())
    if draw and not stop:
        #and not inDone(done.getSize(), okLattice):
        okLattice.add(done.getSize())
        color = goodColors[iColor]
        iColor += 1
        if iColor >= goodColors.getSize():
            iColor = 0
        gRef = Graph("Corner test, " + str(done.getSize()) + " planes", "x", "y", True, "")
        c = Cloud(data)
        c.setPointStyle("bullet")
        gRef.addDrawable(c)
        for il in range(lines.getSize()):
            line = lines[il]
            line.setColor(color)
            gRef.addDrawable(line)
        gRef.draw("Corner" + PRNG + "_" + str(done.getSize()) + "_" + color, 1300, 1200)
        Show(gRef)

