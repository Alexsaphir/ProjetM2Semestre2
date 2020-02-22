import openturns as ot

d = ot.Poisson(3.2)
print([d.getMean()[0], d.getStandardDeviation()[0], \
       d.getSkewness()[0], d.getKurtosis()[0]])
