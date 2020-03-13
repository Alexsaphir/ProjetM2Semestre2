// https://dsp.stackexchange.com/a/56626

#include <cmath>
#include <iostream>

#include "gridfd.h"

double f0(double x, double v, double epsilon, double L)
{
	double k = 2. * M_PI / L;
	return (1. + epsilon * std::cos(k * x)) * std::exp(-v * v / 2.) / std::sqrt(2. * M_PI);
}
int main()
{
	std::cout << "Hello, World!" << std::endl;
	GridFD G(1, 1, 5, 5);
	G.init_f( [](double x, double v){ return f0(x,v,.005, 1.);});
	G.print();

	return 0;
}
