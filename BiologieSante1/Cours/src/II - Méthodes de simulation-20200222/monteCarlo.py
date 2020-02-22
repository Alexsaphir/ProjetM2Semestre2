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
monte_carlo = ot.ProbabilitySimulationAlgorithm(E, ot.MonteCarloExperiment())
monte_carlo.setMaximumOuterSampling(100)
monte_carlo.setBlockSize(100)
monte_carlo.setMaximumCoefficientOfVariation(0.0)
ot.Log.Show(ot.Log.INFO)
monte_carlo.run()
result = monte_carlo.getResult()
# Probability estimate
p = result.getProbabilityEstimate()
print("Probability estimate=", p)
l = result.getConfidenceLength(0.95)
print("Confidence interval 95\%=", ot.Interval(p - 0.5 * l, p + 0.5 * l))
