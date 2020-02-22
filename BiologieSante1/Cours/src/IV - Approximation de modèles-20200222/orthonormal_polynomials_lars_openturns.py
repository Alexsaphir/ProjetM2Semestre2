###################################
# Least-squares approximation on mutlivariate orthonormal polynomial basis
# LASSO with LARS algorithm with regularization's selection using cross validation
###################################

from openturns import *
from openturns.viewer import *
from numpy import *
from numpy.linalg import *
import matplotlib.pyplot as plt

# Function to approximate
def ishigami(x):
    a=1.;
    return [sin(a*x[0]) + 7.*sin(a*x[1])**2 + \
            0.1*(a*x[2])**4*sin(a*x[0])]

d=3
f = PythonFunction(d,1,ishigami)

# Distribution of inputs
DX = ComposedDistribution([Uniform(-1.0,1.0) ,  TruncatedDistribution(Triangular(-1.0,0.,1.0),-0.5, TruncatedDistribution.LOWER), TruncatedDistribution(Triangular(-2.0,0.,2.0),-1.0,1.0)]);
DX.setDescription(["X1","X2","X3"])
X = RandomVector(DX)

graph = DX.getMarginal(1).drawPDF(1024)

view = View(graph)
view.save("marginal" + str(1) + ".png")
view.show()
view.close()

# Training Sample
n = 1000
in_sample = DX.getSample(n)
out_sample = f(in_sample);

# Test sample
ntest = 1000
in_sample_test = DX.getSample(ntest)
out_sample_test = f(in_sample_test)


# Multivariate polynomial basis of total degree p in dimension d
# (orthonormal polynomials with respect to the measure DX)
p = 10
enumerateFunction = LinearEnumerateFunction(d)
H = [StandardDistributionPolynomialFactory(AdaptiveStieltjesAlgorithm(DX.getMarginal(i))) for i in range(3)]
productBasis = OrthogonalProductPolynomialFactory(H,LinearEnumerateFunction(d))
m = enumerateFunction.getStrataCumulatedCardinal(p)
print(m)


# LASSO with Openturns
# LARS algorithm for computing the regularization path, model selection with corrected leave one out
algoSelection = LeastSquaresMetaModelSelectionFactory(LAR(),CorrectedLeaveOneOut())
algo_meta = FunctionalChaosAlgorithm(in_sample,out_sample,DX,FixedStrategy(productBasis,m),LeastSquaresStrategy(algoSelection))
algo_meta.run()
result = algo_meta.getResult()
meta_model = result.getMetaModel()
meta_model_postprocessing = FunctionalChaosRandomVector(result)

# Computing errors
in_sample_error = norm(out_sample-meta_model(in_sample))/norm(out_sample)
test_error = norm(out_sample_test-meta_model(in_sample_test))/norm(out_sample_test)

print("in sample error",in_sample_error)
print("test error",test_error)

# Computing statistics
print("Mean",meta_model_postprocessing.getMean()[0])
print("Variance",meta_model_postprocessing.getCovariance()[0,0])

# Computing sensitivity indices
for j in range(d):
    print("Sobol index input " + DX.getDescription()[j] + "=%.2f" % (100.0 * meta_model_postprocessing.getSobolIndex(j)) + "%")
    print("Sobol total index input " + DX.getDescription()[j] + "=%.2f" % (100.0 * meta_model_postprocessing.getSobolTotalIndex(j)) + "%")

