import openturns as ot

data = [[0.2], [1.96], [2.08], [3.01], [3.1], [3.6]]
bw1 = ot.KernelSmoothing().computeSilvermanBandwidth(data)
bw2 = ot.KernelSmoothing().computePluginBandwidth(data)
bw3 = ot.KernelSmoothing().computeMixedBandwidth(data)
est_dist1 = ot.KernelSmoothing().build(data, bw1)
est_dist2 = ot.KernelSmoothing().build(data, bw2)
est_dist3 = ot.KernelSmoothing().build(data, bw3)
