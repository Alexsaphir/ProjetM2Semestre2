from openturns import *
from openturns.viewer import *
from time import *

N = 1000
data = NumericalSample(0, 2)
for n in range(1, 301):
    print "n=", n
    distribution = RandomMixture([Exponential(1.0)]*n)
    sample = distribution.getSample(N)
    t0 = time()
    pdf = distribution.computePDF(sample)
    t1 = time()
    data.add([n, (t1-t0)/N])

g = Graph("Evaluation PDF de $\sum_{k=1}^nE(1.0)$\n", "$n$", "$t$ (s)", True, "", 1, 2)
g.add(Curve(data))
view = View(g)
view.save("bench_poisson.png")
view.close()
