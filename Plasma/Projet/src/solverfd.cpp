#include "solverfd.h"

#include <algorithm>
#include <fstream>
#include <iostream>

#include "grid.h"

SolverFD::SolverFD(double dt, double T, double L, double Vmax, uint Nx, uint Nv)
	: m_dt(dt), m_t(0.), m_T(T), m_Grid(L, Vmax, Nx, Nv)
{
	FluxX = std::vector<double>(m_Grid.getNx(), 0.);
	FluxV = std::vector<double>(m_Grid.getNv(), 0.);
}

SolverFD::SolverFD(double dt, double T, const Grid& Grid) : m_dt(dt), m_t(0.), m_T(T), m_Grid(Grid)
{
	FluxX = std::vector<double>(m_Grid.getNx(), 0.);
	FluxV = std::vector<double>(m_Grid.getNv(), 0.);
}

void SolverFD::computeFluxV(uint i)
{
#pragma omp parallel for default(shared)
	for (int j = 0; j < m_Grid.getNv(); j++)
	{
		FluxV.at(j) = (m_Grid.f(i, j + 1) - m_Grid.f(i, j));
	}
}

void SolverFD::computeFluxX(uint j)
{
#pragma omp parallel for default(shared)
	for (int i = 0; i < m_Grid.getNx(); i++)
	{
		FluxX.at(i) = (m_Grid.f(i + 1, j) - m_Grid.f(i, j));
	}
}

void SolverFD::stepTransportV(double dt)
{
	double dv = m_Grid.getDv();
	const double Nv = m_Grid.getNv();

	for (int i = 0; i < m_Grid.getNx(); i++)
	{
		computeFluxV(i);
#pragma omp parallel for
		for (int j = 1; j < m_Grid.getNv(); j++)
		{
			m_Grid.f(i, j) += (dt / dv)
							  * (std::max(0., m_Grid.E(i)) * FluxV.at(j - 1)
								 + std::min(0., m_Grid.E(i)) * FluxV.at(j));
		}
		m_Grid.f(i, 0) += (dt / dv)
						  * (std::max(0., m_Grid.E(i)) * FluxV.at(Nv - 1)
							 + std::min(0., m_Grid.E(i)) * FluxV.at(0));
	}
}

void SolverFD::stepTransportX(double dt)
{
	double dx = m_Grid.getDx();
	double Nx = m_Grid.getNx();

	for (int j = 0; j < m_Grid.getNv(); j++)
	{
		computeFluxX(j);
#pragma omp parallel for
		for (int i = 1; i <m_Grid.getNx(); ++i)
		{
			m_Grid.f(i, j) -= (dt / dx)
							  * (std::max(0., m_Grid.getV(j)) * FluxX.at(i - 1)
								 + std::min(0., m_Grid.getV(j)) * FluxX.at(i));
		}

		m_Grid.f(0, j) -= (dt / dx)
						  * (std::max(0., m_Grid.getV(j)) * FluxX.at(Nx - 1)
							 + std::min(0., m_Grid.getV(j)) * FluxX.at(0));
	}
}

// Avant d'appeler les différences finies on a juste besoin d'initialiser f
void SolverFD::solve()
{
	std::ofstream out("ElectricEnergy.csv", std::ofstream::out | std::ofstream::trunc);
	while (m_t < m_T)
	{
		double dt;
		m_Grid.computeElectricField();
		out << m_t << ',' << m_Grid.electricEnergy() << '\n';
		dt = std::min({m_Grid.getDv() / m_Grid.maxElectricField(), m_Grid.getDx() / m_Grid.getVmax(), m_dt});

		stepTransportV(dt);
		stepTransportX(dt);
		m_t += dt;
		if (!m_Grid.getSym())
			std::cout << "Le temps écoulé : " << m_t << std::endl;
	}
}
void SolverFD::save(const std::string& filename) const
{
	m_Grid.save(filename);
}
