from openturns import *
from math import *

# Physical model
f = NumericalMathFunction(["x1", "x2"], ["y"], ["1.0 / (exp(-2*x1)+exp(-2*x2))"])
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
# FORM
form = FORM(AbdoRackwitz(), E, X.getMean())
f.enableHistory()
form.run()
result = form.getResult()
# Post-analytical
algo = PostAnalyticalImportanceSampling(result)
f.clearHistory()
algo.setMaximumOuterSampling(100)
algo.setBlockSize(100)
algo.setMaximumCoefficientOfVariation(0.0)
algo.run()
result = algo.getResult()
# Probability estimate
p = result.getProbabilityEstimate()
print "Probability estimate=", p
l = result.getConfidenceLength(0.95)
print result.getVarianceEstimate()
print "Confidence interval 95%=", Interval(p - 0.5 * l, p + 0.5 * l)
print f.getHistoryInput().getSample().getSize()
graph = Graph("Post-analytical, s=" + str(s) + ", p=" + str(p), "x1", "x2", True, "")
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
graph.draw("post_analytical.png", 600, 610)
Show(graph)
