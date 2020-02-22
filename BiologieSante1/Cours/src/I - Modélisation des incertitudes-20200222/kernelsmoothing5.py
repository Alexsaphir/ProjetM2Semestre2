import openturns as ot
data = [[0.2], [1.96], [2.08], [3.01], [3.1], [3.6]]
ks = ot.KernelSmoothing()
bw3 = ks.computeMixedBandwidth(data)
est_dist2 = ks.build(data, bw3)
ks.setBoundaryCorrection(True)
est_dist1 = ks.build(data, bw3)
bw1 = ks.computeSilvermanBandwidth(data)
est_dist3 = ks.build(data, bw1)
