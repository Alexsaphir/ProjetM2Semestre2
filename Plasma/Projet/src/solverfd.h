#ifndef SOLVERFD_H
#define SOLVERFD_H

#include "gridfd.h"

class solverFD
{
public:
	solverFD(double dt, double T, double L, double Vmax, uint Nx, uint Nv);
	solverFD(double dt, double T, GridFD &Grid);

	// calcul flux
	std::vector<double> Fi(uint i);
	std::vector<double> Fj(uint j);

	// Difference fini en deux étapes avec un splitting
	void computefUnDemi();
	void computeNextf();

	// encapsulation on va de t=0 à T
	void computeFD();

private:
	double m_dt{.00001};
	double m_t{0.};
	double m_T{1.};

	GridFD m_Grid;
};

#endif // SOLVERFD_H
