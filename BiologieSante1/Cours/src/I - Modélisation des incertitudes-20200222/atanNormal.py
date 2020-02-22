import openturns as ot

sigma = 0.5
distribution = ot.Normal(0.0, sigma).atan()
ot.Show(distribution.drawPDF())
