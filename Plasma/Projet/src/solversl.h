
#ifndef SOLVERSL_H
#define SOLVERSL_H

#include "grid.h"

class SolverSL
{
public:
	SolverSL(double dt, double T, double L, double Vmax, uint Nx, uint Nv);
	SolverSL(double dt, double T, const Grid& Grid);

	void solve();

	void save(const std::string& filename) const;

private:
	void stepTransportV(double dt);
	void stepTransportX(double dt);

	std::pair<int, double> getPAlpha(double s, double delta, double dt) const;

private:
	double m_dt {.01};
	double m_t {0.};
	double m_T {1.};

	Grid m_Grid;
	std::vector<double> m_fX;
	std::vector<double> m_fV;
};

#endif // SOLVERSL_H
