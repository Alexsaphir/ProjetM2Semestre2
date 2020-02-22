import openturns as ot

data = [[0.2], [1.96], [2.08], [3.01], [3.1], [3.6]]
est_dist = ot.KernelSmoothing(ot.Normal()).build(data)
est_dist = ot.KernelSmoothing(ot.Triangular()).build(data)
est_dist = ot.KernelSmoothing(ot.Epanechnikov()).build(data)
