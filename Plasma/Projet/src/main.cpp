// https://dsp.stackexchange.com/a/56626

#include <cmath>
#include <iostream>

#include "grid.h"
#include "solverfd.h"

double f0(double x, double v, double epsilon, double L)
{
	double k = 2. * M_PI / L;
//	return std::exp(-v*v/2 - (x-.5)*(x-.5)/(2.*.01))/(M_PI/5.);
	return (1. + epsilon * std::cos(k * x)) * std::exp(-v * v / 2.) / std::sqrt(2. * M_PI);

}
int main()
{
	std::cout << std::floor(-8./3.);
	return 0;

	Grid G(4.*M_PI, 10., 100, 500);
	G.init_f([](double x, double v) { return f0(x, v, .01, 4.*M_PI); });
	G.computeElectricField();
//	G.save("out.csv");

	SolverFD S(0.1, 25, G);
	S.solve();
	S.save("out.csv");



	return 0;
}
