from openturns import *

##################################################
# CAS q petit
# S1 = S2

# loi discrete
support = [[0.0], [0.2], [0.4], [0.6], [0.8]]
probabilities = [0.2]*5
loiUnifDisc = ot.UserDefined(support, probabilities)

# Loi uniforme
loiUnif = Uniform(0,1)


cdf_loiUnifDisc = loiUnifDisc.drawCDF(0, 1, 1001)
draw_cdf_loiUnifDisc = cdf_loiUnifDisc.getDrawable(0)
draw_cdf_loiUnifDisc.setLegend('loi S1/S2')
draw_cdf_loiUnifDisc.setColor('blue')
Show(Graph(cdf_loiUnifDisc))


#CDF  Loi uniforme
cdf_loiUnif = loiUnif.drawCDF(0, 1, 1001)
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
draw_cdf_loiUnif.setLegend('loi continue')
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
draw_cdf_loiUnif.setLegend('loi continue')
cdf_loiUnif.setLegendPosition('bottomright')
cdf_loiUnif.setDrawable(draw_cdf_loiUnifDisc, 0)
cdf_loiUnif.add(draw_cdf_loiUnif)
cdf_loiUnif.setTitle('Loi discrete S1 / S2')
cdf_loiUnif.setXTitle('x')

Show(Graph(cdf_loiUnif))
cdf_loiUnif.draw('cdf_loiUnif')

#PDF  Loi uniforme
pdf_loiUnifDisc = loiUnifDisc.drawPDF(0, 1, 1001)
draw_pdf_loiUnifDisc = pdf_loiUnifDisc.getDrawable(0)
draw_pdf_loiUnifDisc.setLegend('loi S1/S2')
draw_pdf_loiUnifDisc.setColor('blue')
pdf_loiUnifDisc.setDrawable(draw_pdf_loiUnifDisc,0)
pdf_loiUnifDisc.setTitle('Loi discrete S1 / S2')
pdf_loiUnifDisc.setXTitle('x')

Show(Graph(pdf_loiUnifDisc))
pdf_loiUnif.draw('pdf_loiUnif')


##################################################
# CAS q grand
# S1 != S2

# loi discrete
# S2
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
draw_pdf_loiUnifDisc.setLegendName('loi S2')
draw_pdf_loiUnifDisc.setColor('blue')



# loi discrete
# S1
support = [[1.0 + i / N] for i in range(N)]
probabilities = [0.5**i for i in range
loiUnifDisc = ot.UserDefined(support, probabilities)
K = 16
weight = 1.0
for k in range(K):
  for l in range(N):
    support[l] = 0.5 * support[l]
    pairColl.add(UserDefinedPair(NumericalPoint(1, support[l]), weight))
  weight = 0.5 * weight

loiUnifDiscS1 = UserDefined(pairColl)

pdf_loiUnifDiscS1 = loiUnifDiscS1.drawPDF(0, 1, 1024)
draw_pdf_loiUnifDiscS1 = pdf_loiUnifDiscS1.getDrawable(0)
draw_pdf_loiUnifDiscS1.setLegendName('loi S1')
draw_pdf_loiUnifDiscS1.setColor('green')
pdf_loiUnifDiscS1.setDrawable(draw_pdf_loiUnifDiscS1, 0)
pdf_loiUnifDiscS1.addDrawable(draw_pdf_loiUnifDisc)
pdf_loiUnifDiscS1.setTitle('Loi discrete S1 / S2')
pdf_loiUnifDiscS1.setXTitle('x')

Show(Graph(pdf_loiUnifDiscS1))
pdf_loiUnifDiscS1.draw('pdf_loiS1_S2')

# CDF

cdf_loiUnifDiscS1 = loiUnifDiscS1.drawCDF(0, 1, 1024)

Show(Graph(cdf_loiUnifDiscS1))

draw_cdf_loiUnifDiscS1 = cdf_loiUnifDiscS1.getDrawable(0)
draw_cdf_loiUnifDiscS1.setLegendName('loi S1')
draw_cdf_loiUnifDiscS1.setColor('green')

# Loi uniforme
cdf_loiUnif = loiUnif.drawCDF(0, 1, 1024)
cdf_loiUnif.setTitle('Loi discrete S1 / S2')
cdf_loiUnif.setXTitle('x')
draw_cdf_loiUnif = cdf_loiUnif.getDrawable(0)
draw_cdf_loiUnif.setLegendName('loi continue')
draw_cdf_loiUnif.setColor('red')
cdf_loiUnif.setDrawable(draw_cdf_loiUnif,0)
cdf_loiUnif.setLegendPosition('bottomright')
cdf_loiUnif.addDrawable(draw_cdf_loiUnifDisc)

cdf_loiUnif.addDrawable(draw_cdf_loiUnifDiscS1)

Show(Graph(cdf_loiUnif))
cdf_loiUnif.draw('cdf_loiS1_S2')

