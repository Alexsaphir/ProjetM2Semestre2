from openturns import *

def drawPDF3D(copula, N, clip=2.0, solid=True, as_field=True, eps=0.0):
    if solid:
        mesh = IntervalMesher([N, N, 1]).build(Interval([eps]*3, [1.0-eps]*3))
        sample = mesh.getVertices().getMarginal([0, 1])
    else:
        mesh = IntervalMesher([N, N]).build(Interval([eps]*2, [1.0-eps]*2))
        sample = mesh.getVertices()
    pdf = copula.computePDF(sample)
    n = sample.getSize()
    vertices = NumericalSample(n, 3)
    if not solid:
        simplices = mesh.getSimplices()
        for i in range(simplices.getSize()):
            simplex = simplices[i]
            simplex.add(simplex[2])
            simplices[i] = simplex
        mesh.setSimplices(simplices)
    for i in range(n):
        val = min(clip, pdf[i, 0])
        vertices[i, 0] = sample[i, 0]
        vertices[i, 1] = sample[i, 1]
        vertices[i, 2] = val
        pdf[i, 0] = val
    mesh = Mesh(vertices, mesh.getSimplices())
    if as_field:
        return Field(mesh, pdf)
    else:
        return mesh

def convertFieldAsMesh3D(field, clip=2.0, as_field = True):
    mesh = field.getMesh()
    sample = mesh.getVertices()
    values = field.getValues()
    n = sample.getSize()
    vertices = NumericalSample(n, 3)
    simplices = mesh.getSimplices()
    for i in range(simplices.getSize()):
        simplex = simplices[i]
        simplex.add(simplex[2])
        simplices[i] = simplex
    mesh.setSimplices(simplices)
    for i in range(n):
        val = min(clip, values[i, 0])
        vertices[i, 0] = sample[i, 0]
        vertices[i, 1] = sample[i, 1]
        vertices[i, 2] = val
        values[i, 0] = val
    mesh = Mesh(vertices, mesh.getSimplices())
    if as_field:
        return Field(mesh, values)
    else:
        return mesh
