from openturns import *
import math as m

class CsiszarDivergence:
    
    def __init__(self, f, cXkY):
        self.f_ = f
        self.cXkY_ = cXkY
        self.input_ = NumericalSample(0, 2)

    def computeKernel(self, uv):
        cXY = self.cXkY_.computePDF(uv)
        return self.f_([1.0 / cXY]) * cXY

    def getValue(self):
        integrand = NumericalMathFunction(PythonFunction(2, 1, self.computeKernel))
        integrand.enableHistory()
        value = IteratedQuadrature().integrate(integrand, Interval([0.0]*2, [1.0]*2))[0]
        self.input_ = integrand.getHistoryInput()
        return value

    def getInput(self):
        return self.input_.getSample()

