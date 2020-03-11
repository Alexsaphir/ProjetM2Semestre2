//https://dsp.stackexchange.com/a/56626

#include <iostream>

#include "gridfd.h"

double ma_fonction(double e,double x,double v );

struct A{
	double a{10};
	double b{20};
};
int main()
{
	std::cout << "Hello, World!" << std::endl;
	GridFD G(1, 1, 3, 3);
	G.print();
	double c,d;

	return 0;
}
