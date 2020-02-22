from openturns import *
from openturns.viewer import *
from math import *
from time import time
from visu_pdf import *
from csiszar import *

# Here we load the database
in_dim = 24
out_dim = 12
data = NumericalSample.ImportFromCSVFile("Deliver.csv")
indices_input = Indices(in_dim)
indices_input.fill()
in_data = data.getMarginal(indices_input)
in_description = ["X"+str(i).zfill(2) for i in range(in_dim)]
in_data.setDescription(in_description)
print data.getSize()

indices_output = Indices(out_dim)
indices_output.fill(in_dim, 1)
out_data = data.getMarginal(indices_output)
out_description = ["Y"+str(i).zfill(2) for i in range(out_dim)]
out_data.setDescription(out_description)

names = ["KL", "Hell", "TV", "Pear", "Ney"]
formulas = ["t*log(t)", "(sqrt(t) - 1)^2", "abs(t - 1)", "(t - 1)^2", "(t - 1)^2 / t"]

result = Tensor(out_dim, in_dim, len(names))

for j in range(out_dim):
    print "#"*50
    print "j=", j
    yData = out_data.getMarginal(j)
    yName = out_description[j]
    for i in range(in_dim):
        xName = in_description[i]
        data = in_data.getMarginal(i)
        data.stack(yData)
        cXY = BernsteinCopulaFactory().build(data)
        g = cXY.drawPDF([0.0]*2, [1.0]*2)
        g.setTitle(yName + " vs " + xName)
        g.setXTitle(xName)
        g.setYTitle(yName)
        view = View(g)
        view.save("C_" + xName + "_" + yName + ".png")
        view.close()
        pdf3d = drawPDF3D(cXY, 256, 5.0, False, True, 1e-9)
        pdf3d.exportToVTKFile("C_" + xName + "_" + yName + ".vtk")
        print "i=", i
#        for k in range(len(names)):
#            t0 = time()
#            print xName, yName, names[k]
#            algo = CsiszarDivergence(NumericalMathFunction("t", formulas[k]), cXY)
#            S = algo.getValue()
#            print "S_" + names[k] + "=", S, time() - t0, "s"
#            result[j, i, k] = S
#    print
#    for k in range(len(names)):
#        sample = NumericalSample(out_dim, in_dim)
#        for m in range(out_dim):
#            for n in range(in_dim):
#                sample[m, n] = result[m, n, k]
#        sample.exportToCSVFile(names[k] + ".csv")
#        g = SensitivityAnalysis.DrawImportanceFactors(sample[j], in_description, "Sensitivity " + out_description[j] + ", " + names[k])
#        view = View(g)
#        view.save(out_description[j] + "_" + names[k] + ".png")
#        view.close()
