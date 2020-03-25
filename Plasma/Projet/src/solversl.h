
#ifndef SOLVERSL_H
#define SOLVERSL_H

#include "grid.h"

class SolverSL
{
public:
	SolverSL(double dt, double T, double L, double Vmax, uint Nx, uint Nv);
	SolverSL(double dt, double T, Grid& Grid);

	void solve();

	void save(const std::string& filename) const;

private:
	void stepTranportV(double dt);
	void stepTransportX(double dt);

	std::pair<int, double> getPAlpha(double s, double delta, double dt) const;

private:
	double m_dt {.00001};
	double m_t {0.};
	double m_T {1.};

	Grid m_Grid;

};

#endif // SOLVERSL_H
