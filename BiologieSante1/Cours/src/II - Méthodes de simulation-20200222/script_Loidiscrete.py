
import openturns as ot


# loi discrete
support = [[0.0], [0.2], [0.4], [0.6], [0.8]]
probabilities = [0.2]*5
loiUnifDisc = ot.UserDefined(support, probabilities)

pdf_loiUnifDisc = loiUnifDisc.drawPDF(0, 1, 1001)
draw_pdf_loiUnifDisc = pdf_loiUnifDisc.getDrawable(0)
draw_pdf_loiUnifDisc.setLegend('loi discrete')
draw_pdf_loiUnifDisc.setColor('blue')
#Show(pdf_loiUnifDisc)



# Loi uniforme
loiUnif = ot.Uniform(0,1)
pdf_loiUnif = loiUnif.drawPDF(-0.1, 1.4, 5001)
pdf_loiUnif.setTitle('PDF Loi continue / Loi discrete')
pdf_loiUnif.setXTitle('u')
draw_pdf_loiUnif = pdf_loiUnif.getDrawable(0)
draw_pdf_loiUnif.setLegend('loi continue')
pdf_loiUnif.setDrawable(draw_pdf_loiUnif,0)
pdf_loiUnif.add(draw_pdf_loiUnifDisc)
#Show(pdf_loiUnif)
pdf_loiUnif.draw('pdf_loiUnif')




cdf_loiUnifDisc = loiUnifDisc.drawCDF(0, 1, 1001)
draw_cdf_loiUnifDisc = cdf_loiUnifDisc.getDrawable(0)
draw_cdf_loiUnifDisc.setLegend('loi discrete')
draw_cdf_loiUnifDisc.setColor('blue')
#Show(cdf_loiUnifDisc)


# Loi uniforme
cdf_loiUnif = loiUnif.drawCDF(0, 1, 1001)
cdf_loiUnif.setTitle('CDF Loi S1 / S2')
cdf_loiUnif.setXTitle('u')
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
draw_cdf_loiUnif.setLegend('loi continue')
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
cdf_loiUnif.setLegendPosition('bottomright')
cdf_loiUnif.setDrawable(draw_cdf_loiUnifDisc, 0)
cdf_loiUnif.add(draw_cdf_loiUnif)
#Show(cdf_loiUnif)
cdf_loiUnif.draw('cdf_loi_qpetit_S1_S2')
