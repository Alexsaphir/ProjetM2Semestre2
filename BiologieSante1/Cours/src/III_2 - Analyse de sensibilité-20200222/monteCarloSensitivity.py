import openturns as ot
import openturns.viewer as otv

# Physical model
f = ot.SymbolicFunction(["x1", "x2"], ["1.0 / (exp(-2*x1)+exp(-4*x2))"])
######################################
# Mandatory for sensitivity analysis #
######################################
f.enableHistory()
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
# Monte Carlo
algo = ot.ProbabilitySimulationAlgorithm(E, ot.MonteCarloExperiment())
algo.setMaximumOuterSampling(10000)
algo.setMaximumCoefficientOfVariation(0.0)
algo.run()
result = algo.getResult()
# Probability
p = result.getProbabilityEstimate()
print("Probabilite Monte Carlo=", p)
sensitivity = ot.SimulationSensitivityAnalysis(result)
print("Point moyen de l'evenement=", sensitivity.computeMeanPointInEventDomain())
print("Facteurs d'importance=", sensitivity.computeImportanceFactors())
graph = sensitivity.drawImportanceFactorsRange()
view = otv.View(graph)
view.save("Monte_carlo_sensitivity.png")
view.close()
