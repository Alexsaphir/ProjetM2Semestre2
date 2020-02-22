from openturns import *
from math import *

U = Uniform(0.0, 1.0)
V = Uniform(0.0, 1.0)

N = (-2.0 * U.log()).sqrt() * (2.0 * pi * V).cos()
Show(N.drawPDF(-4.0, 4.0))
