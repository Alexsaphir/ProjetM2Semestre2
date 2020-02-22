import openturns as ot
from openturns.viewer import *

dim = 10
size = 2**14
doe = ot.LowDiscrepancyExperiment(ot.SobolSequence(dim), ot.ComposedDistribution([ot.Uniform(0.0, 1.0)]*dim), size)

sample = doe.generate()

for i in range(dim):
    x_i = "x_" + str(i)
    for j in range(i+1, dim):
        x_j = "x_" + str(j)
        graph = ot.Graph("Sobol sequence dim=" + str(dim) + " (" + x_i + ", " + x_j + ")", x_i, x_j, True, "")
        cloud = ot.Cloud(sample.getMarginal([i, j]))
        cloud.setPointStyle("dot")
        view = View(cloud)
        view.save("Sobol_" + x_i + "_" + x_j + ".png")
        view.close()
