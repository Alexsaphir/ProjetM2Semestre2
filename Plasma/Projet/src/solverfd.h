#ifndef SOLVERDF_H
#define SOLVERDF_H

#include "gridfd.h"

class solverDF
{
public:
	solverDF(double dt, double T, double L, double Vmax, uint Nx, uint Nv);

	void solveE();

private:
	double m_dt{.00001};
	double m_t{0.};
	double m_T{1.};

	GridFD m_Grid;
};

#endif // SOLVERDF_H
