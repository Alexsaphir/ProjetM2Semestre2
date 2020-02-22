import openturns as ot

data = ot.Sample.ImportFromCSVFile('data.csv')
print(data)

# Maximum likelihood estimation
estimated_distribution = ot.UniformFactory().build(data)
print('Estimated uniform=', estimated_distribution)

# Best distribution wrt an entropy criterion
models = [ot.UniformFactory(), ot.NormalFactory(), ot.ExponentialFactory()]
best_distribution = ot.FittingTest.BestModelBIC(data, models)
print('Selected distribution=', best_distribution)
