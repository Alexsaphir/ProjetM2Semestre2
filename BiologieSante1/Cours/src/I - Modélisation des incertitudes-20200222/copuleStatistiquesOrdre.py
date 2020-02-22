import openturns as ot

size = 1000
marginales1 = [ot.Uniform(0.0, 1.0), ot.Uniform(0.25, 1.25)]
marginales2 = [ot.Triangular(0.0, 0.5, 1.0), ot.Mixture([ot.Triangular(0.25, 0.5, 0.6), ot.Triangular(0.75, 1.0, 1.4)])]

distribution1 = ot.MaximumEntropyOrderStatisticsDistribution(marginales1)
copule1 = ot.MaximumEntropyOrderStatisticsCopula(marginales1)
distribution2 = ot.MaximumEntropyOrderStatisticsDistribution(marginales2)
copule2 = ot.MaximumEntropyOrderStatisticsCopula(marginales2)

for d in [distribution1, copule1, distribution2, copule2]:
    graph = d.drawPDF()
    graph.setColors(['red'])
    cloud = ot.Cloud(d.getSample(size))
    cloud.setColor('blue')
    graph.add(cloud)
    ot.Show(graph)
