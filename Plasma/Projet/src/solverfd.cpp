#include "solverfd.h"
#include "gridfd.h"
#include <iostream>

solverFD::solverFD(double dt, double T, double L, double Vmax, uint Nx, uint Nv)
	: m_dt(dt), m_t(0.), m_T(T), m_Grid(L, Vmax, Nx, Nv)
{
}

solverFD::solverFD(double dt, double T, GridFD &Grid) : m_dt(dt), m_t(0.), m_T(T), m_Grid(Grid) {}

std::vector<double> solverFD::Fi(uint i)
{
	std::vector<double> F;

	F.resize(m_Grid.getNv(), 0.);

	for (uint j = 0; j < m_Grid.getNv(); j++) {
		F.at(j) = (m_Grid.f(i, j + 1) - m_Grid.f(i, j));
	}

	return F;
}

std::vector<double> solverFD::Fj(uint j)
{
	std::vector<double> F;

	F.resize(m_Grid.getNx(), 0.);

	for (uint i = 0; i < m_Grid.getNx(); i++) {
		F.at(i) = (m_Grid.f(i + 1, j) - m_Grid.f(i, j));
	}

	return F;
}

void solverFD::computefUnDemi()
{
	double dv = m_Grid.getDv();
	double Nv = m_Grid.getNv();
	double maxE;
	std::vector<double> F;

	maxE = m_Grid.maxElectricField();

	m_dt = dv / maxE;

	for (uint i = 0; i < m_Grid.getNx() + 1; i++) {
		F = this->Fi(i);
		for (uint j = 0; j < m_Grid.getNv() - 1; j++) {
			m_Grid.f(i, j) = m_Grid.f(i, j)
							 + (m_dt / dv)
								   * (std::max(0., m_Grid.E(i)) * F.at(j)
									  + std::min(0., m_Grid.E(i)) * F.at(j + 1));
		}

		// f n+1/2 en 0 avec f périodique sur [-Vmax,Vmax]
		m_Grid.f(i, 0) = m_Grid.f(i, 0)
						 + (m_dt / dv)
							   * (std::max(0., m_Grid.E(i)) * F.at(Nv - 1)
								  + std::min(0., m_Grid.E(i)) * F.at(0));
		// f n+1/2 en Nv avec f périodique sur [-Vmax,Vmax]
		m_Grid.f(i, Nv) = m_Grid.f(i, Nv)
						  + (m_dt / dv)
								* (std::max(0., m_Grid.E(i)) * F.at(Nv - 1)
								   + std::min(0., m_Grid.E(i)) * F.at(0));
	}

	m_t += m_dt;
}

void solverFD::computeNextf()
{
	double dx = m_Grid.getDx();
	double Nx = m_Grid.getNx();
	std::vector<double> F;

	m_dt = dx / m_Grid.getVmax();

	for (uint j = 0; j < m_Grid.getNv(); j++) {
		F = this->Fj(j);
		for (uint i = 1; i < m_Grid.getNx() - 1; i++) {
			m_Grid.f(i, j) = m_Grid.f(i, j)
							 - (m_dt / dx)
								   * (std::max(0., m_Grid.getV(j)) * F.at(i)
									  + std::min(0., m_Grid.getV(j)) * F.at(i + 1));
		}
		m_Grid.f(0, j) = m_Grid.f(0, j)
						 - (m_dt / dx)
							   * (std::max(0., m_Grid.getV(j)) * F.at(Nx - 1)
								  + std::min(0., m_Grid.getV(j)) * F.at(0));
		m_Grid.f(Nx, j) = m_Grid.f(Nx, j)
						  - (m_dt / dx)
								* (std::max(0., m_Grid.getV(j)) * F.at(Nx - 1)
								   + std::min(0., m_Grid.getV(j)) * F.at(0));
	}

	m_t = m_t + m_dt;
}

// Avant d'appeler les différences finies on a juste besoin d'initialiser f
void solverFD::computeFD()
{
	while (m_t < m_T) {
		m_Grid.print();
		m_Grid.computeElectricCharge();
		m_Grid.computeElectricField();
		this->computefUnDemi();
		this->computeNextf();
		std::cout << "Le temps passe : " << m_t << std::endl;
	}

	m_Grid.print();
}
