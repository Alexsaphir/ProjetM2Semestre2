import openturns as ot
import openturns.viewer as otv

d1 = ot.Normal([-3.0, 3.0], ot.CovarianceMatrix(2, [4.0, 1.0, 1.0, 9.0]))
d2 = ot.Normal([3.0, 3.0], ot.CovarianceMatrix(2, [9.0, -1.0, -1.0, 4.0]))
d3 = ot.Normal([0.0, -3.0], ot.CovarianceMatrix(2, [4.0, 0.0, 0.0, 4.0]))
distribution = ot.Mixture([d1, d2, d3], [0.3, 0.3, 0.4])
copule = ot.SklarCopula(distribution)
graph = distribution.drawPDF([512]*2)
graph.setTitle("")
graph.setXTitle("")
graph.setYTitle("")
graph.setLegendPosition("")
graph.draw("mixture.pdf", 600, 620)
graph = copule.drawPDF([512]*2)
graph.setTitle("")
graph.setXTitle("")
graph.setYTitle("")
graph.setLegendPosition("")
graph.draw("sklar.pdf", 600, 620)
