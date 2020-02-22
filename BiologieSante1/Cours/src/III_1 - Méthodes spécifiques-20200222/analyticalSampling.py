import openturns as ot

# Physical model
f = ot.SymbolicFunction(["x1", "x2"], ["1.0 / (exp(-2*x1)+exp(-2*x2))"])
# Input distribution
dist_x = ot.Normal(2)
# Input random vector
X = ot.RandomVector(dist_x)
# Output random vector
Y = ot.RandomVector(f, X)
# Event
s = 4.0
test = ot.Greater()
E = ot.Event(Y, test, s)
# FORM
form = ot.FORM(ot.AbdoRackwitz(), E, X.getMean())
form.run()
result = form.getResult()
# Post-analytical controled importance sampling
algo = ot.PostAnalyticalControlledImportanceSampling(result)
algo.setMaximumOuterSampling(100)
algo.setBlockSize(100)
algo.setMaximumCoefficientOfVariation(0.0)
algo.run()
result = algo.getResult()
# Probability
p = result.getProbabilityEstimate()
print("Probability estimate=", p)
l = result.getConfidenceLength(0.95)
print("Confidence interval 95\%=", ot.Interval(p - 0.5 * l, p + 0.5 * l))
