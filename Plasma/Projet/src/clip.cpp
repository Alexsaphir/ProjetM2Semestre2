#include <cmath>
#include <fstream>
#include <iostream>

#include "grid.h"
#include "solverfd.h"
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

int main()
{
	// Sauvegarde d'une solution sous forme d'un clip video, largement posible de faire mieux
	Grid G(5, 5., 512 - 1, 512, true);
	G.init_f([](double x, double v) { return fx(0., x, v); });

	for (int i = 1; i < 50; i++)
	{
		std ::cout << i * 2 << "%\n";
		SolverSL S(.01, i / 10., G);
		S.solve();
		S.save("clip.csv." + std::to_string(i));
	}

	return 0;
}
