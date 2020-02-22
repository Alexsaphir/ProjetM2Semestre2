import openturns as ot
# Physical model
f = ot.NumericalMathFunction(["x1", "x2"], ["y"], ["1.0 / (exp(-2*x1)+exp(-2*x2))"])
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
# Probability
p = result.getEventProbability()
print "Probability FORM=", p
# SORM
sorm = ot.SORM(ot.AbdoRackwitz(), E, X.getMean())
sorm.run()
result = sorm.getResult()
# Probability
p = result.getEventProbabilityBreitung()
print "Probability SORM=", p
