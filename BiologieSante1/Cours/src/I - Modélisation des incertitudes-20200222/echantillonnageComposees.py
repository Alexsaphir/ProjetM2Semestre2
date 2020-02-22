import openturns as ot

size = 500
copule1 = ot.IndependentCopula(2)
copule2 = ot.NormalCopula(ot.CorrelationMatrix(2, [1.0, 0.5, 0.5, 1.0]))
copule3 = ot.ClaytonCopula(2.5)
liste_marginales = [[ot.Normal()]*2, [ot.Gamma(2.0, 1.0)]*2]
for copule in [copule1, copule2, copule3]:
    for marginales in liste_marginales:
        distribution = ot.ComposedDistribution(marginales, copule)
        sample = distribution.getSample(size)
        graph = distribution.drawPDF(sample.getMin(), sample.getMax())
        cloud = ot.Cloud(sample)
        cloud.setColor('red')
        cloud.setPointStyle('bullet')
        graph.add(cloud)
        ot.Show(graph)
