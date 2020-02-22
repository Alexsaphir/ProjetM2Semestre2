####################################
#  Kernel ridge & Gaussian processes (Kriging)
####################################

from openturns import *
import numpy as np
from math import *
from random import uniform

f = NumericalMathFunction('x', 'exp(-5.*x^2)+x/3.')
x_1 = -1
x_2 = 1
n = 10

# Initializing the seed
RandomGenerator.SetSeed(int(uniform(0,2^32-1)))
distrib = Uniform(x_1,x_2)
x = distrib.getSample(n)

# Noisy evaluations at sample points
Noise =Normal(0.,0.01);
y = f(x) + Noise.getSample(n);

# Basis functions (features)
h = 0.1
functions = [NumericalMathFunction('x', \
	 'exp(-(x-' + str(x[i])[1:-1] + ')^2/(' + str(h) + '^2))') \
	 for i in range(n)]
basis = Basis(functions)

# Regularization parameter and Kernel matrix
l = 0.1
K = CovarianceMatrix(n)
for r in range(n):
	for s in range(n):
		K[r,s] = exp(-(np.array(x[r,:])-np.array(x[s,:]))*\
			(np.array(x[r,:])-np.array(x[s,:]))/(h*h))

# Weights (all equal to 1 here)
w = NumericalPoint(n, 1.0)

# Indices (all activated)
indices = Indices(n)
indices.fill()

# Kernel Ridge regression
algorithm = PenalizedLeastSquaresAlgorithm(x,y,w,basis,indices,l,K)
algorithm.run()
coef = algorithm.getCoefficients()
f_t = NumericalMathFunction(functions,coef)

# Display of the results
graph = f.draw(x_1,x_2,1024)
graph.add(Cloud(x,y))
graph.add(f_t.draw(x_1,x_2,1024))
graph.setColors(['blue','blue','red'])
Show(graph)


# Gaussian processes (Kriging)
basisPrior = ConstantBasisFactory().build()
covarianceModel = SquaredExponential(1)
algo = KrigingAlgorithm(x, y, basisPrior, covarianceModel)
algo.run()
result = algo.getResult()
f_kriging = result.getMetaModel()

graph = f.draw(x_1,x_2,1024)
graph.add(Cloud(x,y))
graph.add(f_kriging.draw(x_1,x_2,1024))
#graph2.add(Cloud(x,f_t(x)))
graph.setColors(['blue','blue','red'])
graph.setTitle('')
Show(graph)
