from openturns import *

s = 0.0005
data = Sample.ImportFromCSVFile("LCGdata"+str(s)+".csv")
g = Graph("Corner test, Linear Congruential Generator, a=" + str(s), "x", "y", True, "")
c = Cloud(data)
c.setPointStyle("bullet")
g.add(c)
g.draw("CornerLCG"+str(s), 1300, 1200)
Show(g)
