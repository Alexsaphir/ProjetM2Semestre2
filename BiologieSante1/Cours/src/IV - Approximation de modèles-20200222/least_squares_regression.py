####################################
# Least-squares regression (linear approximation)
####################################

from openturns import *
import numpy as np
f = NumericalMathFunction("x"," exp(-5*x^2)+x/3","y")

n = 15
X = Uniform(-1,1)
x = X.getSample(n)
y = f(x);

m=5

functions = [NumericalMathFunction("x", "x^" + str(i)) \ 
	for i in range(m)]  
basis = Basis(functions)

indices = Indices(m)
indices.fill()
algorithm = PenalizedLeastSquaresAlgorithm(x,y, basis,indices)
algorithm.run()
coefficients = algorithm.getCoefficients()
fls = NumericalMathFunction(functions, coefficients)

graph = f.draw(-1,1, 1024)
graph.add(Cloud(x,y))
graph.add(fls.draw(-1,1, 1024))
graph.setColors(["black", "red", "red"])
graph.setTitle("")
graph.setLegends(["function", "data", "approximation"])
Show(graph)  



