from openturns import *
from visu_pdf import *

N = 10000
M = 377
alpha = 0.95

mesh = IntervalMesher([100]*2).build(Interval([1.0e-6]*2, [1.0-1.0e-6]*2))
vertices = mesh.getVertices()
process_sample = ProcessSample(mesh, 0, 1)

for n in range(N):
    print "n=", n
    data = IndependentCopula(2).getSample(M)
    copula = BernsteinCopulaFactory().build(data)
    values = copula.computePDF(vertices)
    process_sample.add(values)
    # Export the upper bound
    q_upper = process_sample.computeQuantilePerComponent(0.5 * (1.0 + alpha))
    convertFieldAsMesh3D(q_upper, 2.0, False).exportToVTKFile("Independent_upper.vtk")
    # Export the low bound
    q_lower = process_sample.computeQuantilePerComponent(0.5 * (1.0 - alpha))
    convertFieldAsMesh3D(q_lower, 2.0, False).exportToVTKFile("Independent_lower.vtk")
