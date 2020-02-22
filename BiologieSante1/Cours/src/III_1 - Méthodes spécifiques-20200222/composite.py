from openturns import *

for i in range(6):
    eps = (i+1) * 0.05
    atom = Uniform(eps, 1+eps)
    f = NumericalMathFunction("x", "x*sin(1.0/x)")
    d = CompositeDistribution(f, atom)
    g = d.drawPDF(1024)
    g.setLegendPosition("")
    g.setTitle(str(d))
    Show(g)
    g.draw("Composite_" + str(i) + ".pdf")
