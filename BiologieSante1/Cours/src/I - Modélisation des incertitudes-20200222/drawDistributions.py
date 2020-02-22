import openturns as ot

all = [ot.Uniform(), ot.Normal(), ot.Exponential(), \
       ot.Bernoulli(), ot.Geometric(), ot.Poisson()]

for distribution in all:
    ot.Show(distribution.drawPDF())
    ot.Show(distribution.drawCDF())
