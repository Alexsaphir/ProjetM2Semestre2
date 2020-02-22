from openturns import *


# loi discrete
pairColl = UserDefinedPairCollection(5, UserDefinedPair(NumericalPoint(1), 0.0))
x1 = NumericalPoint(1)
pairColl[0] = UserDefinedPair(x1, 0.2)
x2= NumericalPoint(1, 0.2)
pairColl[1] = UserDefinedPair(x2,0.2)
x3 = NumericalPoint(1, 0.4)
pairColl[2] = UserDefinedPair(x3, 0.2)
x4 = NumericalPoint(1, 0.6)
pairColl[3] = UserDefinedPair(x4, 0.2)
x5 = NumericalPoint(1, 0.8)
pairColl[4] = UserDefinedPair(x5, 0.2)
loiUnifDisc = UserDefined(pairColl)

pdf_loiUnifDisc = loiUnifDisc.drawPDF(0, 1, 1001)
draw_pdf_loiUnifDisc = pdf_loiUnifDisc.getDrawable(0)
draw_pdf_loiUnifDisc.setLegendName('loi discrete')
draw_pdf_loiUnifDisc.setColor('blue')
#Show(pdf_loiUnifDisc)



# Loi uniforme
loiUnif = Uniform(0,1)
pdf_loiUnif = loiUnif.drawPDF(-0.1, 1.4, 5001)
pdf_loiUnif.setTitle('PDF Loi continue / Loi discrete')
pdf_loiUnif.setXTitle('u')
draw_pdf_loiUnif = pdf_loiUnif.getDrawable(0)
draw_pdf_loiUnif.setLegendName('loi continue')
pdf_loiUnif.setDrawable(draw_pdf_loiUnif,0)
pdf_loiUnif.addDrawable(draw_pdf_loiUnifDisc)
#Show(pdf_loiUnif)
pdf_loiUnif.draw('pdf_loiUnif')




cdf_loiUnifDisc = loiUnifDisc.drawCDF(0, 1, 1001)
draw_cdf_loiUnifDisc = cdf_loiUnifDisc.getDrawable(0)
draw_cdf_loiUnifDisc.setLegendName('loi discrete')
draw_cdf_loiUnifDisc.setColor('blue')
#Show(cdf_loiUnifDisc)


# Loi uniforme
cdf_loiUnif = loiUnif.drawCDF(0, 1, 1001)
cdf_loiUnif.setTitle('CDF Loi S1 / S2')
cdf_loiUnif.setXTitle('u')
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
draw_cdf_loiUnif.setLegendName('loi continue')
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
cdf_loiUnif.setLegendPosition('bottomright')
cdf_loiUnif.setDrawable(draw_cdf_loiUnifDisc, 0)
cdf_loiUnif.addDrawable(draw_cdf_loiUnif)
#Show(cdf_loiUnif)
cdf_loiUnif.draw('cdf_loi_qpetit_S1_S2')