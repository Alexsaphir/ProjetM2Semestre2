###################################
# Sparse approximation: greedy algorithms and LASSO
###################################

from openturns import *
from openturns.viewer import *
from numpy import *
from numpy.linalg import *
from sklearn import linear_model
import matplotlib.pyplot as plt

# Function to approximate
def ishigami(x):
    a=1.;
    return [sin(a*x[0]) + 7.*sin(a*x[1])**2 + \
            0.1*(a*x[2])**4*sin(a*x[0])]

d=3
f = PythonFunction(d,1,ishigami)

# Sample
X = ComposedDistribution([Uniform(-1,1)]*d);
n = 100
in_sample = X.getSample(n)
out_sample = f(in_sample);
y=array(out_sample).T[0] #numpy format

# Test sample
ntest = 100
in_sample_test = X.getSample(ntest)
out_sample_test = f(in_sample_test)
ytest=array(out_sample_test).T[0] #numpy format


# Multivariate polynomial basis of total degree p in dimension d
# (tensorization of Legendre bases)
p = 4
enumerateFunction = LinearEnumerateFunction(d)
H = [LegendreFactory()]*d
productBasis = OrthogonalProductPolynomialFactory(H,LinearEnumerateFunction(d))
m = enumerateFunction.getStrataCumulatedCardinal(p)
print(m)


# Matrix of evaluations of basis functions
functions = [productBasis.build(i) for i in range(m)]
A = zeros((n,m))
for i in range(m):
    A[:,i]=array(functions[i](in_sample)).T[0]
Atest = zeros((ntest,m))
for i in range(m):
    Atest[:,i]=array(functions[i](in_sample_test)).T[0]

# Orthogonal Mathching Pursuit (Orthogonal greedy algorithm)
r=5
omp = linear_model.OrthogonalMatchingPursuit(n_nonzero_coefs=r, \
                                             tol=None, fit_intercept=True, normalize=True,precompute=True)
omp.fit(dot(A.T,A), dot(A.T,y))
coefs_omp = omp.coef_

Ir, = coefs_omp.nonzero()
print(Ir)
# Computing errors
in_sample_error = norm(y-dot(A,coefs_omp))/norm(y)
test_error = norm(ytest-dot(Atest,coefs_omp))/norm(ytest)
print("in_sample_error",in_sample_error)
print("test_error",test_error)

# Solving LASSO with LARS (Computing the regularization path)
_, _, coefs = linear_model.lars_path(A, y, method='lasso', verbose=True)

# re-estimating coefficients with ordinary least-squares
ols = True
if ols:
    for i in range(coefs.shape[1]):
        Ir,=coefs[:,i].nonzero()
        temp = solve(dot(A[:,Ir].T,A[:,Ir]),dot(A[:,Ir].T,y))
        coefs[Ir,i] = temp

# Plotting the regularization path
xx = sum(abs(coefs.T), axis=1)
xx = xx/max(xx)
plt.plot(xx, coefs.T)
ymin, ymax = plt.ylim()
plt.vlines(xx, ymin, ymax, linestyle='dashed')
plt.xlabel('|coef| / max|coef|')
plt.ylabel('Coefficients')
plt.title('LASSO Path')
plt.axis('tight')
plt.show()

# Computing errors
in_sample_errors = [ norm(y-dot(A,coefs[:,i]))/norm(y) \
                    for i in range(coefs.shape[1])]
test_errors = [ norm(ytest-dot(Atest,coefs[:,i]))/norm(ytest) \
               for i in range(coefs.shape[1])]

plt.semilogy(in_sample_errors)
plt.semilogy(test_errors,'r')
plt.xlabel("model")
plt.legend(["in sample error","test error"])



