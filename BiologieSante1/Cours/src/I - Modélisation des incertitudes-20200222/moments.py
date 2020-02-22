import openturns as ot

distribution = ot.Normal(1.0, 3.0)
print("mean=", distribution.getMean())
print("std =", distribution.getStandardDeviation())
print("skew=", distribution.getSkewness())
print("kurt=", distribution.getKurtosis())
print("E(X^6)=", distribution.getShiftedMoment(6, [0.0]))
print("E((X-3)^6)=", distribution.getShiftedMoment(6, [3.0]))
