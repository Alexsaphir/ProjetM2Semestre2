from openturns import *
from math import *

# Physical model
f = NumericalMathFunction(["x1", "x2"], ["y"], ["1.0 / (exp(-x1)^2+exp(-x2)^2)"])
# Input distribution
dist_x = Normal(2)
# Input random vector
X = RandomVector(dist_x)
# Output random vector
Y = RandomVector(f, X)
# Event
s = 4.0
test = Greater()
E = Event(Y, test, s)
# Simulation
imp_samp = ImportanceSampling(E, Normal([1.0]*2, IdentityMatrix(2)))
imp_samp.setMaximumOuterSampling(100)
imp_samp.setBlockSize(100)
imp_samp.setMaximumCoefficientOfVariation(0.0)
Log.Show(Log.INFO)
f.enableHistory()
imp_samp.run()
result = imp_samp.getResult()
# Probability estimate
p = result.getProbabilityEstimate()
print("Probability estimate=", p)
l = result.getConfidenceLength(0.95)
print(result.getVarianceEstimate())

print("Confidence interval 95%=", Interval(p - 0.5 * l, p + 0.5 * l))
graph = Graph("Importance sampling, s=" + str(s) + ", p=" + str(p), "x1", "x2", True, "")
x_sample = f.getHistoryInput().getSample()
y_sample = f.getHistoryOutput().getSample()
domain = f.draw([-4.5]*2, [4.5]*2).getDrawable(0)
domain.setLevels([s])
domain.setLabels([str(s)])
domain.setColor("red")
domain.setLineWidth(2)
graph.add(domain)
good_sample = NumericalSample(0, 2)
bad_sample = NumericalSample(0, 2)
for i in range(x_sample.getSize()):
    if test(y_sample[i, 0], s):
        good_sample.add(x_sample[i])
    else:
        bad_sample.add(x_sample[i])
c = Cloud(good_sample)
c.setColor("red")
c.setPointStyle("bullet")
graph.add(c)
c = Cloud(bad_sample)
c.setColor("blue")
c.setPointStyle("bullet")
graph.add(c)
graph.draw("is.png", 600, 610)
Show(graph)
