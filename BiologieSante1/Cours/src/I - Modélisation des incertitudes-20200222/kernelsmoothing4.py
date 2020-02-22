import openturns as ot

data = [[0.2], [1.96], [2.08], [3.01], [3.1], [3.6]]
ks = ot.KernelSmoothing()
est_dist1 = ks.build(data)
ks.setBoundaryCorrection(True)
est_dist2 = ks.build(data)
