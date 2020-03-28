#include <cmath>
#include <fstream>
#include <iostream>

#include "grid.h"
#include "solversl.h"

// Cas analytique
double fx0(double x, double v)
{
	return std::exp(-10. * x * x) * std::exp(-v * v);
}
double fx(double t, double x, double v)
{
	return std::exp(-10. * std::pow(x * std::cos(t) - v * std::sin(t), 2))
		   * std::exp(-std::pow(x * std::sin(t) + v * std::cos(t), 2));
}

double errSolSL(int N, double dt, double T)
{
	Grid G(5, 5., N - 1, N, true);

	G.init_f([](double x, double v) { return fx(0., x, v); });

	SolverSL S(dt, T, G);
	S.solve();
	G.init_f([](double x, double v) { return fx(1., x, v); });
	return S.diffMax(G);
}

void studyConv()
{
	std::ofstream out("convergence_0.csv", std::ofstream::out | std::ofstream::trunc);
	double		  T = 1.;
	for (auto N : {128, 256, 512, 1024, 2048})
	{
		for (auto dt : {1., .1, .05, .01, .005, .001})
		{
			std::cout << N << ',' << dt << ',' << '\n';
			out << N << ',' << dt << ',' << errSolSL(N, dt, T) << '\n';
		}
	}
}

int main()
{
	//Sauvegarde d'une solution
	Grid G(5, 5., 512 - 1, 512, true);
	G.init_f([](double x, double v) { return fx(0., x, v); });
	SolverSL S(.1, 2., G);
	S.solve();
	G.init_f([](double x, double v) { return fx(1., x, v); });

	studyConv();
	return 0;
}