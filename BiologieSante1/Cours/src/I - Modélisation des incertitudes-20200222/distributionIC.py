import openturns as ot

data = ot.NumericalSample.ImportFromCSVFile('data.csv')
# Maximum likelihood estimation
estimation_result = ot.NormalFactory().buildEstimator(data)
print 'Estimated distribution=', estimation_result.getDistribution()
param_dist = estimation_result.getParameterDistribution()
print 'Estimator distribution=', param_dist
RC_95 = param_dist.computeMinimumVolumeLevelSet(0.95)
print 'RC 95%=\n', RC_95
IC_95 = param_dist.computeMinimumVolumeInterval(0.95)
print 'IC 95%=\n', IC_95
ot.ResourceMap.SetAsBool('IntervalMesher-UseDiamond', True);
graph_RC_95 = ot.LevelSetMesher([50]*2).build(RC_95, param_dist.getRange()).draw()
graph_RC_95.setColors(['red'])
graph_IC_95 = ot.IntervalMesher([25]*2).build(IC_95).draw()
graph_IC_95.setColors(['blue'])
graph = graph_IC_95
graph.add(graph_RC_95)
graph.setAutomaticBoundingBox(True)
graph.setLegends([''])
graph.setTitle(r'$(\mu,\sigma)$ estimator')
graph.setXTitle(r'$\mu$')
graph.setYTitle(r'$\sigma$')
ot.Show(graph)

