// https://dsp.stackexchange.com/a/56626

#include <cmath>
#include <fstream>
#include <iostream>

#include "grid.h"
#include "solverfd.h"
#include "solversl.h"

// Landau
double f0(double x, double v, double epsilon, double L)
{
	double k = 2. * M_PI / L;
	//	return std::exp(-v*v/2 - (x-.5)*(x-.5)/(2.*.01))/(M_PI/5.);
	return (1. + epsilon * std::cos(k * x)) * std::exp(-v * v / 2.) / std::sqrt(2. * M_PI);
}
// Cette fonction permet de tracer la pente de décroissance
void curveRef(double T, std::string filename)
{
	// Courbe reference
	std::ofstream out(filename, std::ofstream::out | std::ofstream::trunc);
	double		  t = 0.;
	while (t < T)
	{
		out << t << ',' << std::exp(-.1533 * t - 3.5) << '\n';
		t += .1;
	}
}


int main()
{
	Grid G(4. * M_PI, 10., 700, 700);
	Grid G_FD(4. * M_PI, 10., 200, 200);

	// Initialise les 2 grilles
	// Obligé de recopier a la main T dans le lambda
	//Besoin d'implementer un constructeur prenant un functionnal
	G.init_f([](double x, double v) { return f0(x, v, 0.01, 4. * M_PI); });
	G_FD.init_f([](double x, double v) { return f0(x, v, 0.01, 4. * M_PI); });

	/* Fonction carré pour tester le transport */
	//	 G.init_f([](double x, double v) {
	//		if (std::abs(x-2.) < 1. && std::abs(v-2.) < 1.)
	//			return 1./4.;
	//		return 0.;
	//	});

	double T = 5;
	SolverFD S(0.05, T, G_FD);
	SolverSL SL(0.1, T, G);
	S.solve();
	SL.solve();

	S.save("out_fd.csv");
	SL.save("out_sl.csv");

	curveRef(T,"ref.csv");

	return 0;
}
