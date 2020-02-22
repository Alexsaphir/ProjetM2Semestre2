from openturns import *

s = 0.0005
data = Sample.ImportFromCSVFile("MTdata"+str(s)+".csv")
g = Graph("Corner test, Mersenne Twister, a=" + str(s), "x", "y", True, "")
c = Cloud(data)
c.setPointStyle("bullet")
g.add(c)
g.draw("CornerMT"+str(s), 1300, 1200)
Show(g)
