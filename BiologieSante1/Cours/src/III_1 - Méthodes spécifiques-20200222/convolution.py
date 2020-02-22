from openturns import *
from math import *

iMax = 7
palette = Drawable.BuildDefaultPalette(iMax)

atom = Normal(0.0, 1.0)
mean = atom.getMean()[0]

sum = atom
g = Graph("TCL, atom=" + str(atom), "x", "pdf", True, "topright")
for i in range(iMax):
    N = 2**i
    if i == 0:
        sum = atom
    else:
        sum = sum + sum
    d = (sum / N - mean) * sqrt(N)
    print d
    drawable = d.drawPDF(1024).getDrawable(0)
    drawable.setLegend("N=" + str(N))
    drawable.setColor(palette[i])
    g.add(drawable)
ref = Normal([0.0], atom.getCovariance()).drawPDF().getDrawable(0)
ref.setColor("black")
ref.setLineStyle("dashed")
ref.setLegend("limit")
g.add(ref)
Show(g)
g.draw("TCL_" + atom.getClassName() + ".pdf")
