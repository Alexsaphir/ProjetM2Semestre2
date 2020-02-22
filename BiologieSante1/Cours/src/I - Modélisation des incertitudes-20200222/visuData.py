from openturns import *

PRNG = "Random"
threshold = 0.0001
data = NumericalSample.ImportFromCSVFile("data" + PRNG + ".csv")

g = Graph("Corner test, " + PRNG, "x", "y", True, "")
c = Cloud(data)
c.setPointStyle("bullet")
g.addDrawable(c)
g.draw("Corner" + PRNG + str(threshold))
