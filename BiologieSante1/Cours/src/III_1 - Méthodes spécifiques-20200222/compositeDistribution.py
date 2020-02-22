import openturns as ot
import openturns.viewer as otv
# Les deux graphes a produire
graph_atan = ot.Graph("Densite Y", "y", "pdf", True, "topright")
graph_sin = ot.Graph("Densite Y", "y", "pdf", True, "topright")
# Pour chaque valeur de sigma
for sigma in [0.2, 0.5, 1.0, 2.0, 5.0]:
# Distribution de l'arctangente d'une Normal
    dist_Y = ot.Normal(0.0, sigma).atan()
    graph_atan.add(dist_Y.drawPDF(-5.0, 5.0, 512))
    # Distribution du sinus d'une Normal
    dist_Y = ot.Normal(0.0, sigma).sin()
    graph_sin.add(dist_Y.drawPDF(-5.0, 5.0, 512))
    graph_atan.setColors(ot.Drawable.BuildDefaultPalette(5))
    graph_sin.setColors(ot.Drawable.BuildDefaultPalette(5))
    view = otv.View(graph_atan)
    view.save("Normal_atan.png")
    view.close()
    view = otv.View(graph_sin)
    view.save("Normal_sin.png")
    view.close()
