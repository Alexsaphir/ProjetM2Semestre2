#include "solversl_keen.h"

#include <cmath>
#include <fstream>
#include <iostream>

SolverKeen::SolverKeen(double dt, double T, double L, double Vmax, uint Nx, uint Nv)
	: m_dt(dt), m_t(0.), m_T(T), m_Grid(L, Vmax, Nx, Nv)
{
	m_fX.resize(m_Grid.getNx(), 0.);
	m_fV.resize(m_Grid.getNv(), 0.);
}

SolverKeen::SolverKeen(double dt, double T, Grid Grid) : m_dt(dt), m_t(0.), m_T(T), m_Grid(std::move(Grid))
{
	m_fX.resize(m_Grid.getNx(), 0.);
	m_fV.resize(m_Grid.getNv(), 0.);
}

void SolverKeen::stepTransportV(double dt)
{
	for (int i = 0; i < m_Grid.getNx(); ++i)
	{
		double e = m_Grid.E(i) + Eapp(m_t, m_Grid.getX(i));

// On calcule les valeurs des débuts des caractéristiques
#pragma omp parallel for
		for (int j = 0; j < m_Grid.getNv(); ++j)
		{
			auto [p, alpha] = getPAlpha(-e, m_Grid.getDv(), dt);
			m_fV.at(j)		= (1. - alpha) * m_Grid.f(i, j - p) + alpha * m_Grid.f(i, j - p - 1);
		}
// On écrit dans la grille a la fin car
// on peut profiter de la périodicité de Grid
#pragma omp parallel for
		for (int j = 0; j < m_Grid.getNv(); ++j)
			m_Grid.f(i, j) = m_fV.at(j);
	}
}

void SolverKeen::stepTransportX(double dt)
{
	for (int j = 0; j < m_Grid.getNv(); ++j)
	{
		double v = m_Grid.getV(j);
#pragma omp parallel for
		for (int i = 0; i < m_Grid.getNx(); ++i)
		{
			auto [p, alpha] = getPAlpha(v, m_Grid.getDx(), dt);
			m_fX.at(i)		= (1. - alpha) * m_Grid.f(i - p, j) + alpha * m_Grid.f(i - p - 1, j);
		}
		// On écrit dans la grille a la fin car
		// on peut profiter de la périodicité de Grid
#pragma omp parallel for
		for (int i = 0; i < m_Grid.getNx(); ++i)
			m_Grid.f(i, j) = m_fX.at(i);
	}
}

void SolverKeen::solve()
{
	std::ofstream out("ElectricEnergySL.csv", std::ofstream::out | std::ofstream::trunc);
	// On calcule le champ électrique pr la condition initiale
	m_Grid.computeElectricField();
	while (m_t <= m_T)
	{
		// Save énergie Électrique
		out << m_t << ',' << m_Grid.electricEnergy() << '\n';

		// Split step

		stepTransportV(.5 * m_dt);
		stepTransportX(m_dt);
		m_Grid.computeElectricField();
		stepTransportV(.5 * m_dt);

		m_t += m_dt;
		std::cout << "Le temps écoulé : " << m_t << std::endl;
	}
}

void SolverKeen::save(const std::string& filename) const
{
	m_Grid.save(filename);
}

std::pair<int, double> SolverKeen::getPAlpha(double s, double delta, double dt) const
{
	double r = s * dt / delta; // nb de deplacement en maille
	int	   p = std::floor(r);
	return {p, r - p};
}
double SolverKeen::diffMax(const Grid& G) const
{
	return m_Grid.diffMax(G);
}
double SolverKeen::alpha(double t) const
{
	double epsilon = .5 * (std::tanh((t0 - tL) / tw) - std::tanh((t0 - tR) / tw));
	double epst	   = .5 * (std::tanh((t - tL) / tw) - std::tanh((t - tR) / tw));
	return (epst - epsilon) / (1. - epsilon);
}
double SolverKeen::Eapp(double t, double x)
{
	return Emax * k * alpha(t) * std::sin(k * x - w * t);
}
void SolverKeen::savediff(const Grid& G, const std::string& filename) const
{
	std::ofstream os(filename, std::ofstream::out | std::ofstream::trunc);
	for (uint i = 0; i < G.getNx() + 1; i += 10)
	{
		double x = G.getX(i);
		for (uint j = 0; j < G.getNv() + 1; j += 10)
		{
			double v = G.getV(j);
			os << x << ',' << v << ',' << m_Grid.f(i, j) - G.f(i, j) << ',' << m_Grid.f(i, j) << ',' << G.E(i)
			   << ',' << G.Rho(i) << '\n';
		}
	}
}
