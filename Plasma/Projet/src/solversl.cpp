
#include "solversl.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>

SolverSL::SolverSL(double dt, double T, double L, double Vmax, uint Nx, uint Nv)
{
}

SolverSL::SolverSL(double dt, double T, Grid& Grid)
{
}

void SolverSL::stepTranportV(double dt)
{
}

void SolverSL::stepTransportX(double dt)
{
}

void SolverSL::solve()
{
	std::ofstream out("ElectricEnergy.csv", std::ofstream::out | std::ofstream::trunc);
	while (m_t < m_T)
	{
	}
}

void SolverSL::save(const std::string& filename) const
{
}

std::pair<int, double> SolverSL::getPAlpha(double s, double delta, double dt) const
{
	double r = s * dt / delta;
	int	   p = std::floor(r);
	return {p, r - p};
}
