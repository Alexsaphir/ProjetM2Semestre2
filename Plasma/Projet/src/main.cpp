// https://dsp.stackexchange.com/a/56626

#include <cmath>
#include <iostream>

#include "grid.h"
#include "solverfd.h"
#include "solversl.h"

#include <complex.h>
#include <fftw3.h>
#include <functional>
#include <numeric>

double f0(double x, double v, double epsilon, double L)
{
	double k = 2. * M_PI / L;
	//	return std::exp(-v*v/2 - (x-.5)*(x-.5)/(2.*.01))/(M_PI/5.);
	return (1. + epsilon * std::cos(k * x)) * std::exp(-v * v / 2.) / std::sqrt(2. * M_PI);
}

double fx0(double x, double v)
{
	return std::exp(-10. * x * x) * std::exp(-v * v);
}
double fx(double t, double x, double v)
{
	return std::exp(-10. * std::pow(x * std::cos(t) - v * std::sin(t), 2))
		   * std::exp(-std::pow(v * std::cos(t) + x * std::sin(t), 2));
}

double errSolSL(int N, double dt, double T)
{
	Grid G(5, 5., N - 1, N, true);
	Grid Ge(5, 5., N - 1, N, true);

	G.init_f(fx0);
	Ge.init_f([](double x, double v) { return fx(1., x, v); });

	SolverSL S(dt, T, G);
	S.solve();

	return Ge.diffMax(G);
}

void studyConv()
{
	double T  = 1.;
	double dt = .01;
	std::cout << "Erreur espace : \n";
	for (auto N : {128, 256, 512, 8192, 16384})
	{
		std::cout << "N=" << N << " : " << errSolSL(N, dt, T) << std::endl;
	}
	return;
	std::cout << "Erreur Temps : \n";
	for (auto dt : {.1, .01, .001})
	{
		std::cout << "dt=" << dt << ": " << errSolSL(2048, dt, T) << std::endl;
	}
}


int main()
{
	Grid G(4. * M_PI, 10., 1000, 1000);
	G.init_f([](double x, double v) { return f0(x, v, 0.01, 4. * M_PI); });

	/* Fonction carré pour tester le transport */
//	 G.init_f([](double x, double v) {
//		if (std::abs(x-2.) < 1. && std::abs(v-2.) < 1.)
//			return 1./4.;
//		return 0.;
//	});


	//	SolverFD S(0.01, 25., G);
	SolverSL SL(0.1, 10., G);
	//	S.solve();
	SL.solve();
	SL.save("out.csv");

	return 0;
}
