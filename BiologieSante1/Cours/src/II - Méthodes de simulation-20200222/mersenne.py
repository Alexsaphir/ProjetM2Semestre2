from openturns import *
from openturns.viewer import *
from time import *

# Taille de l'echantillon
size = 500
# Acces direct, flux 1D
t0 = time()
data = RandomGenerator.Generate(size)
t1 = time()
print("t=", size / (t1 - t0), "s")
# Acces via une distribution, flux 1D ou nD
dim = 2
distribution = ComposedDistribution([Uniform(0.0, 1.0)]*dim)
t0 = time()
data = distribution.getSample(size/2)
t1 = time()
print("t=", size / (t1 - t0), "s")
# Acces sous forme de DOE, flux 1D ou nD
doe = MonteCarloExperiment(distribution, size)
data = doe.generate()
t1 = time()
print("t=", size / (t1 - t0), "s")
graph = Graph("Mersenne twister dim=" + str(dim), "x", "y", True, "")
graph.add(Cloud(data))
view = View(graph)
view.save("mersenne.png")
view.close()
Show(graph)
