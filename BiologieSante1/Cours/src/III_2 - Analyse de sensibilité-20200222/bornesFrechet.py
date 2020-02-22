from openturns import *

rho_min = NumericalMathFunction("sigma", "(exp(-sigma)-1)/(sqrt(_e-1)*sqrt(exp(sigma^2)-1))")
rho_max = NumericalMathFunction("sigma", "(exp(sigma)-1)/(sqrt(_e-1)*sqrt(exp(sigma^2)-1))")
g = Graph("", "sigma", "rho", True, "")
c = rho_min.draw(0.0000001, 5.0).getDrawable(0)
c.setColor("red")
g.add(c)
c = rho_max.draw(0.0000001, 5.0).getDrawable(0)
c.setColor("red")
g.add(c)
g.add(Curve([[0.0, 0.0], [5.0, 0.0]]))
g.setGridColor("black")
g.draw("BornesFrechet")
