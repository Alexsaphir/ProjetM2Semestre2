import openturns as ot

data = [[0.2], [1.96], [2.08], [3.01], [3.1], [3.6]]
estimated_distribution = ot.KernelSmoothing().build(data)
