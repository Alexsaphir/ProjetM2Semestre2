// https://dsp.stackexchange.com/a/56626

#include <cmath>
#include <iostream>

#include "gridfd.h"
#include "solverfd.h"

double f0(double x, double v, double epsilon, double L)
{
	double k = 2. * M_PI / L;
	return (1. + epsilon * std::cos(k * x)) * std::exp(-v * v / 2.) / std::sqrt(2. * M_PI);
}
int main()
{
	std::cout << "Hello, World!" << std::endl;
	GridFD G(1., 10., 5, 5);
	G.init_f([](double x, double v) { return f0(x, v, .005, 1.); });
	solverFD S(0.00001, 1., G);
	S.computeFD();

	return 0;
}
