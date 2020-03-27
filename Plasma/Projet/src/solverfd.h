#ifndef SOLVERFD_H
#define SOLVERFD_H

#include "grid.h"

// Cette classe implémenta la résolution utilisant des différences finies

class SolverFD
{
public:
	SolverFD(double dt, double T, double L, double Vmax, uint Nx, uint Nv);
	SolverFD(double dt, double T, const Grid& Grid);

	// encapsulation on va de t=0 à T
	void solve(); // Résouds le problème

	void save(const std::string& filename) const;

private:
	// calcul des flux permettant de faire du in-place
	void computeFluxV(uint i);
	void computeFluxX(uint j);

	// Difference fini en deux étapes avec un splitting
	void stepTransportV(double dt);
	void stepTransportX(double dt);

private:
	double m_dt {.00001};
	double m_t {0.};
	double m_T {1.};

	Grid				m_Grid;
	std::vector<double> FluxX;
	std::vector<double> FluxV;
};

#endif // SOLVERFD_H
