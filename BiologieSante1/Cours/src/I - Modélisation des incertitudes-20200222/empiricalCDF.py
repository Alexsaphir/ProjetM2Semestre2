from openturns import *
# Taille de l'echantillon
size = 500
# Generateur de donnees
ref = Mixture([Normal(0.0, 1.0), Normal(2.0, 0.5)], [0.3, 0.7])
sample = ref.getSample(size)
ref.setDescription(["x"])
graph = ref.drawCDF()
# Loi uniforme discrete sur l'echantillon
empirical = UserDefinedFactory().build(sample)
graph.add(empirical.drawCDF())
graph.add(Cloud(sample, Sample(size, 1)))
graph.setColors(["red", "blue", "green"])
graph.setLegends(["reference", "empirique", "data"])
graph.setTitle("CDF empirique, n=" + str(size))
Show(graph)
