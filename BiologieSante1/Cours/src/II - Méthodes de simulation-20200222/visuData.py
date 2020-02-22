import openturns as ot

PRNG = "Random"
threshold = 0.0001
data = ot.Sample.ImportFromCSVFile("data" + PRNG + ".csv")

g = ot.Graph("Corner test, " + PRNG, "x", "y", True, "")
c = ot.Cloud(data)
c.setPointStyle("bullet")
g.addDrawable(c)
g.draw("Corner" + PRNG + str(threshold))
