#ifndef SOLVERDF_H
#define SOLVERDF_H

#include "gridfd.h"

class solverDF
{
public:
	solverDF();
	solverDF(double dt, double t, double T);

private:
	double m_dt{.00001};
	double m_t{0.};
	double m_T{1.};

	GridFD m_Grid;
};

#endif // SOLVERDF_H
