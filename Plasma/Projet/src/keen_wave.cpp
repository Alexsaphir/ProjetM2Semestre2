// https://dsp.stackexchange.com/a/56626

#include <cmath>
#include <fstream>
#include <iostream>

#include "grid.h"
#include "solversl_keen.h"

// Landau equilibre
double f0(double x, double v, double L)
{
	double k = 2. * M_PI / L;
	return std::exp(-v * v / 2.) / std::sqrt(2. * M_PI);
}
// Cette fonction permet de tracer la pente de décroissance


int main()
{


	Grid G(2.*M_PI/.26, 6., 512-1, 512);

	// Initialise les 2 grilles
	// Obligé de recopier a la main T dans le lambda
	//Besoin d'implementer un constructeur prenant un functionnal
	G.init_f([](double x, double v) { return f0(x, v, 2.*M_PI/.26); });
	double T = 500.;
	SolverKeen SL(0.1, T, G);
	SL.solve();
	SL.savediff(G, "keenwave.csv");

	return 0;
}

