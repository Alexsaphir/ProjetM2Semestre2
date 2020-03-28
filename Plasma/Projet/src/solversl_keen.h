
#ifndef SOLVERSL_KEEN_H
#define SOLVERSL_KEEN_H

#include "grid.h"

class SolverKeen
{
public:
	SolverKeen(double dt, double T, double L, double Vmax, uint Nx, uint Nv);
	SolverKeen(double dt, double T, Grid Grid);

	void solve();

	void   save(const std::string& filename) const;
	void   savediff(const Grid& G, const std::string& filename) const;
	double diffMax(const Grid& G) const;

private:
	void stepTransportV(double dt);
	void stepTransportX(double dt);

	double alpha(double t) const;
	double Eapp(double t, double x);

	std::pair<int, double> getPAlpha(double s, double delta, double dt) const;

private:
	double m_dt {.01};
	double m_t {0.};
	double m_T {1.};

private:
	// param keen
	double t0	= 0.;
	double tL	= 69.;
	double tR	= 307.;
	double tw	= 20.;
	double k	= .26;
	double w	= .37;
	double Emax = .2;
	double L	= 2. * M_PI / k;

	Grid				m_Grid;
	std::vector<double> m_fX;
	std::vector<double> m_fV;
};

#endif // SOLVERSL_Keen_H
