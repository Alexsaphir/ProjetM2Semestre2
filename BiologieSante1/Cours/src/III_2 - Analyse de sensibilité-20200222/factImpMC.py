from openturns import *
from openturns.viewer import ViewImage
from math import *

def MCImportanceFactors(algoMC, name):
    event = algoMC.getEvent()
    threshold = event.getThreshold()
    operator = event.getOperator()
    inputDistribution = event.getAntecedent().getDistribution()
    inputDimension = inputDistribution.getDimension()
    description = inputDistribution.getDescription()
    inputSample = algoMC.getInputStrategy().getSample()
    outputSample = algoMC.getOutputStrategy().getSample()
    size = inputSample.getSize()
    newSample = NumericalSample(0, inputDimension)
    # Select the input points that have lead to failure
    for i in range(size):
        if operator(outputSample[i][0], threshold):
            newSample.add(inputSample[i])
    standardMean = inputDistribution.getIsoProbabilisticTransformation()(newSample).computeMean()
    norm = standardMean.norm2()
    importanceFactors = NumericalPointWithDescription(inputDimension)
    importanceFactors.setDescription(description)
    # Compute the importance factors
    data = NumericalSample(inputDimension, 1)
    labels = Description(inputDimension)
    palette = Description(inputDimension)
    for i in range(inputDimension):
        importanceFactors[i] = standardMean[i] * standardMean[i] / norm
        data[i][0] = importanceFactors[i]
        labels[i] = " : " + str(round(1000 * data[i][0]) * 0.1)+"%"
        if description.getSize() < inputDimension:
            labels[i] = "Component " + str(i) + labels[i]
        else:
            labels[i] = description[i] + labels[i]
        palette[i] = DrawableImplementation.GetValidColors()[i]
    # Draw the importance factors
    graph = Graph("Monte Carlo importance factors - " + name, "", "", False, "")
    drawable = Pie(data)
    drawable.setLabels(labels)
    drawable.setPalette(palette)
    graph.addDrawable(drawable)
    return [importanceFactors, standardMean, graph]

value = 1.0
inV = Description(0)
inV.add("x")
inV.add("y")
outV = Description(0)
outV.add("z")
formula = Description(0)
formula.add("(y-1.5*x)^2 - x^3 - 2*y + (" + str(value) + ")")

model = NumericalMathFunction(inV, outV, formula)

distribution = Normal(2)
distribution.setDescription(Description(["U1", "U2"]))
IV = RandomVector(distribution)
OV = RandomVector(model, IV)
event = Event(OV, ComparisonOperator(Less()), 0.0)
mc = MonteCarlo(event)
mc.setInputOutputStrategy(HistoryStrategy(Full()))
mc.setMaximumOuterSampling(1)
mc.setBlockSize(10000)
mc.setMaximumCoefficientOfVariation(0.0)
Log.Show(Log.ALL)
mc.run()
importanceFactors, standardMean, graph = MCImportanceFactors(mc, "Y>10.0")
print importanceFactors
graph.draw("MCImportanceFactor", 1024, 768, 0)

form = FORM(NearestPointAlgorithm(Cobyla()), event, distribution.getMean())
form.run()
dp = form.getResult().getStandardSpaceDesignPoint()

def drawFunc2D(function, xMin, xMax, yMin, yMax, nX, nY):
    # Limite state curve
    gridX = Box(NumericalPoint(1, nX)).generate()
    gridX.scale(NumericalPoint(1, xMax - xMin))
    gridX.translate(NumericalPoint(1, xMin))
    gridY = Box(NumericalPoint(1, nY)).generate()
    gridY.scale(NumericalPoint(1, yMax - yMin))
    gridY.translate(NumericalPoint(1, yMin))
    levels = NumericalPoint(2)
    levels[0] = nX
    levels[1] = nY
    gridXY = Box(levels).generate()
    s = NumericalPoint(2)
    s[0] = xMax - xMin
    s[1] = yMax - yMin
    gridXY.scale(s)
    t = NumericalPoint(2)
    t[0] = xMin
    t[1] = yMin
    gridXY.translate(t)
    z = function(gridXY)
    c = Contour(gridX, gridY, z, NumericalPoint(1), Description(1), False)
    c.setColor("green")
    c.setLineWidth(2)
    return c

g = Graph("Standard space mean failure point", "u1", "u2", True, "topright")
d = distribution.drawPDF().getDrawable(0)
d.setDrawLabels(False)
d.setLegendName("")
g.addDrawable(d)
g.addDrawable(drawFunc2D(model, -5, 5, -5, 5, 90, 90))
data = NumericalSample(2, 2)
data[0] = NumericalPoint(2, 0.0)
data[1] = dp
c = Curve(data)
c.setColor("red")
c.setLineWidth(2)
c.setLineStyle("dashed")
g.addDrawable(c)
c = Cloud(NumericalSample(1, dp))
c.setColor("red")
c.setLegendName("Design point")
c.setPointStyle("fcircle")
g.addDrawable(c)
data = NumericalSample(2, 2)
data[0] = NumericalPoint(2, 0.0)
data[1] = standardMean
c = Curve(data)
c.setColor("orange")
c.setLineWidth(2)
c.setLineStyle("dashed")
g.addDrawable(c)
c = Cloud(NumericalSample(1, standardMean))
c.setColor("orange")
c.setLegendName("Mean failure point")
c.setPointStyle("fsquare")
g.addDrawable(c)
g.setBoundingBox(Interval(NumericalPoint([-3,-3]),NumericalPoint([3,3])))
g.draw("StandardSpace", 800, 800, 0)
ViewImage(g.getBitmap())
