from openturns import *
from openturns.viewer import *
from time import *

# Taille de l'echantillon
size = 500
# Acces direct, flux 1D
t0 = time()
data = DistFunc.rGamma(0.5, size)
t1 = time()
print("t=", size / (t1 - t0), "s")
# Acces via une distribution, flux 1D ou nD
dim = 2
distribution = ComposedDistribution([Gamma(1.0, 0.5)]*dim)
t0 = time()
data = distribution.getSample(size/2)
t1 = time()
print("t=", size / (t1 - t0), "s")
# Acces sous forme de DOE, flux 1D ou nD
doe = MonteCarloExperiment(distribution, size/2)
t0 = time()
data = doe.generate()
t1 = time()
print("t=", size / (t1 - t0), "s")
graph = Graph("Rejection+squeeze Gamma dim=" + str(dim), "x", "y", True, "")
graph.add(Cloud(data))
view = View(graph)
view.save("gamma.png")
view.close()
Show(graph)
