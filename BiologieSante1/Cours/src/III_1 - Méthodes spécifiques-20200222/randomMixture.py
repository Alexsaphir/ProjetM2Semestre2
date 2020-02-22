import openturns as ot
import openturns.viewer as otv
from time import time

# Nombre d'evaluations de la pdf
N = 1000
data = ot.Sample(0, 2)
nMax = 0
tMax = 0.0
tMin = ot.SpecFunc.MaxScalar
for n in range(1, 1001):
    print("n=", n)
    distribution = ot.RandomMixture([ot.Exponential(i + 1.5) for i in range(n)])
    sample = distribution.getSample(N)
    t0 = time()
    pdf = distribution.computePDF(sample)
    t1 = time()
    data.add([n, (t1 - t0) / N])
    if data[n-1, 1] > tMax:
        tMax = data[n-1, 1]
        nMax = n
    if data[n-1, 1] < tMin:
        tMin = data[n-1, 1]
graph = ot.Graph("Evaluation densite de $W$", "$n$", "$t$ (s)", True, "", 1, 3)
graph.add(ot.Curve(data))
curve = ot.Curve([[nMax, tMin], [nMax, tMax]])
curve.setColor("red")
curve.setLineStyle("dashed")
graph.add(curve)
graph.setXMargin(0.0)
graph.setYMargin(0.0)
view = otv.View(graph)
view.save("bench_poisson.png")
view.close()
