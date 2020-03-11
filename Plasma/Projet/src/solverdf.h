#ifndef SOLVERDF_H
#define SOLVERDF_H

#include "gridfd.h"

class solverDF
{
public:
	struct argSolver
	{
		double dt;
		double t;
		double T;
		argGrid grid;
	};

public:
	solverDF();
	solverDF(argSolver arg);

private:
	double m_dt{.00001};
	double m_t{0.};
	double m_T{1.};

	GridFD m_Grid;
};

#endif // SOLVERDF_H
