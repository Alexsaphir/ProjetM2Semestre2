import openturns as ot

copule1 = ot.IndependentCopula(2)
copule2 = ot.NormalCopula(ot.CorrelationMatrix(2, [1.0, 0.5, 0.5, 1.0]))
copule3 = ot.ClaytonCopula(2.5)
for copule in [copule1, copule2, copule3]:
    ot.Show(copule.drawCDF())
    ot.Show(copule.drawPDF())
