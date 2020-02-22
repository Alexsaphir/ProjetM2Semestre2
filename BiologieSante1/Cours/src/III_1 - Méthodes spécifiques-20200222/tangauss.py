from openturns import *

lSigma = [0.2, 0.5, 1.0, 2.0, 5.0]
palette = Drawable.BuildDefaultPalette(len(lSigma))
g = Graph("Densite Y", "y", "pdf", True, "topright")
for i in range(len(lSigma)):
    sigma = lSigma[i]
    dr = Normal(0.0, sigma).atan().drawPDF(1024).getDrawable(0)
    dr.setColor(palette[i])
    dr.setLegend("sigma=" + str(sigma))
    g.add(dr)
g.draw("TanGauss.pdf")
Show(g)
