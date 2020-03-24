#include "solverfd.h"
#include "gridfd.h"
#include <algorithm>
#include <fstream>
#include <iostream>

SolverFD::SolverFD(double dt, double T, double L, double Vmax, uint Nx, uint Nv) : m_dt(dt), m_t(0.), m_T(T), m_Grid(L, Vmax, Nx, Nv)
{
}

SolverFD::SolverFD(double dt, double T, GridFD& Grid) : m_dt(dt), m_t(0.), m_T(T), m_Grid(Grid)
{
}

std::vector<double> SolverFD::Fi(uint i)
{
	std::vector<double> F;

	F.resize(m_Grid.getNv(), 0.);

	for (uint j = 0; j < m_Grid.getNv(); j++)
	{
		F.at(j) = (m_Grid.f(i, j + 1) - m_Grid.f(i, j));
	}

	return F;
}

std::vector<double> SolverFD::Fj(uint j)
{
	std::vector<double> F;

	F.resize(m_Grid.getNx(), 0.);

	for (uint i = 0; i < m_Grid.getNx(); i++)
	{
		F.at(i) = (m_Grid.f(i + 1, j) - m_Grid.f(i, j));
	}

	return std::move(F);
}

void SolverFD::computefUnDemi(double dt)
{
	double				dv = m_Grid.getDv();
	double				Nv = m_Grid.getNv();
	std::vector<double> F;

	for (uint i = 0; i < m_Grid.getNx() + 1; i++)
	{
		F = this->Fi(i);
		for (uint j = 0; j < m_Grid.getNv() - 1; j++)
		{
			m_Grid.f(i, j) = m_Grid.f(i, j) + (dt / dv) * (std::max(0., m_Grid.E(i)) * F.at(j) + std::min(0., m_Grid.E(i)) * F.at(j + 1));
		}

		// f n+1/2 en 0 avec f périodique sur [-Vmax,Vmax]
		m_Grid.f(i, 0) = m_Grid.f(i, 0) + (dt / dv) * (std::max(0., m_Grid.E(i)) * F.at(Nv - 1) + std::min(0., m_Grid.E(i)) * F.at(0));
		// f n+1/2 en Nv avec f périodique sur [-Vmax,Vmax]
		m_Grid.f(i, Nv) = m_Grid.f(i, Nv) + (dt / dv) * (std::max(0., m_Grid.E(i)) * F.at(Nv - 1) + std::min(0., m_Grid.E(i)) * F.at(0));
	}
}

void SolverFD::computeNextf(double dt)
{
	double				dx = m_Grid.getDx();
	double				Nx = m_Grid.getNx();
	std::vector<double> F;

	for (uint j = 0; j < m_Grid.getNv(); j++)
	{
		F = this->Fj(j);
		for (uint i = 1; i < m_Grid.getNx() - 1; i++)
		{
			m_Grid.f(i, j) = m_Grid.f(i, j)
							 - (dt / dx) * (std::max(0., m_Grid.getV(j)) * F.at(i) + std::min(0., m_Grid.getV(j)) * F.at(i + 1));
		}
		m_Grid.f(0, j) = m_Grid.f(0, j)
						 - (dt / dx) * (std::max(0., m_Grid.getV(j)) * F.at(Nx - 1) + std::min(0., m_Grid.getV(j)) * F.at(0));
		m_Grid.f(Nx, j) = m_Grid.f(Nx, j)
						  - (dt / dx) * (std::max(0., m_Grid.getV(j)) * F.at(Nx - 1) + std::min(0., m_Grid.getV(j)) * F.at(0));
	}
}

// Avant d'appeler les différences finies on a juste besoin d'initialiser f
void SolverFD::computeFD()
{
	std::ofstream out("ElectricEnergy.csv", std::ofstream::out | std::ofstream::trunc);
	while (m_t < m_T)
	{
		double dt;
		//		m_Grid.print();
		m_Grid.computeElectricField();
		out << m_t << ',' << m_Grid.electricEnergy() << '\n';

		std::cout << m_Grid.maxElectricField() << '\n';
		dt = std::min({m_Grid.getDv() / m_Grid.maxElectricField(), m_Grid.getDx() / m_Grid.getVmax(), m_dt});

		computefUnDemi(dt);
			computeNextf(dt);
		m_t +=dt;
		std::cout << "Le temps passe : " << m_t << std::endl;
	}

	//	m_Grid.print();
}
void SolverFD::save(const std::string& filename) const
{
	m_Grid.save(filename);
}
