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
# Simulation
imp_samp = ot.ProbabilitySimulationAlgorithm(E, ot.ImportanceSamplingExperiment(ot.Normal([1.0]*2, ot.IdentityMatrix(2))))
imp_samp.setMaximumOuterSampling(100)
imp_samp.setBlockSize(100)
imp_samp.setMaximumCoefficientOfVariation(0.0)
ot.Log.Show(ot.Log.INFO)
imp_samp.run()
result = imp_samp.getResult()
# Probability estimate
p = result.getProbabilityEstimate()
print("Probability estimate=", p)
l = result.getConfidenceLength(0.95)
print("Confidence interval 95\%=", ot.Interval(p - 0.5 * l, p + 0.5 * l))
