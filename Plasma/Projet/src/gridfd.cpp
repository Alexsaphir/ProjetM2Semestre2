#include "gridfd.h"

#include <iostream>
#include <numeric>

GridFD::GridFD()
{
	this->m_f.resize(m_Nx, std::vector<double>(m_Nv, 0.));
	this->m_E.resize(m_Nx, 0.);
	this->m_rho.resize(m_Nx, 0.);

	dx = 1. / static_cast<double>(m_Nx);
	dv = 1. / static_cast<double>(2. * m_Nv);
}

GridFD::GridFD(double L, double Vmax, uint Nx, uint Nv) : m_L {L}, m_Vmax {Vmax}, m_Nx {Nx}, m_Nv {Nv}
{
	this->m_f.resize(m_Nx, std::vector<double>(m_Nv, 0.));
	this->m_E.resize(m_Nx, 0.);
	this->m_rho.resize(m_Nx, 0.);

	dx = 1. / static_cast<double>(m_Nx);
	dv = 1. / static_cast<double>(2. * m_Nv);
}

// Obtention des dimensions du domaine
double GridFD::getL() const
{
	return m_L;
}

double GridFD::getVmax() const
{
	return m_Vmax;
}

// Obtention des dimension du maillage
uint GridFD::getNx() const
{
	return m_Nx;
}

uint GridFD::getNv() const
{
	return m_Nv;
}

// Obtention
double GridFD::getX(int i) const
{
	return static_cast<double>(i) * m_L * dx;
}

double GridFD::getV(int i) const
{
	return static_cast<double>(i) * m_Vmax * dv - m_Vmax;
}

double GridFD::E(int p) const
{
	return m_E.at(p % m_Nx);
}

double& GridFD::E(int p)
{
	return m_E.at(p % m_Nx);
}

double GridFD::f(int p, int v) const
{
	return m_f.at(p % m_Nx).at(v % m_Nv);
}

double& GridFD::f(int p, int v)
{
	return m_f.at(p % m_Nx).at(v % m_Nv);
}

double GridFD::Rho(int p) const
{
	return m_rho.at(p % m_Nx);
}

double& GridFD::Rho(int p)
{
	return m_rho.at(p % m_Nx);
}

void GridFD::print() const
{
	std::cout << "-------";
	for (int i = 0; i < m_Nx; ++i)
		std::cout << getX(i) << ' ';
	std::cout << '\n';
	for (const auto& V : m_f)
	{
		for (const auto& d : V)
		{
			std::cout << d << ' ';
		}
		std::cout << '\n';
	}
}

void GridFD::init_f(double f0(double x, double v))
{
	for (uint i = 0; i < m_Nx; ++i)
	{
		auto x = getX(i);
		for (uint j = 0; j < m_Nv; ++j)
		{
			auto v			= getV(j);
			m_f.at(i).at(j) = f0(x, v);
		}
	}
}

void GridFD::computeElectricCharge()
{
	for (int i = 0; i < m_Nx; ++i)
	{
		double sum	= std::accumulate(m_f.at(i).begin(), m_f.at(i).end(), 0.);
		m_rho.at(i) = dv * sum;
	}
}

void GridFD::computeElectricField()
{
	m_E.at(0) = 0.;
	for (int i = 1; i < m_Nx; ++i)
	{
		m_E.at(i) = m_E.at(i - 1) + dx * (1. - m_rho.at(i));
	}

	// Calcul la moyenne
	double avg = std::accumulate(m_E.begin(), m_E.end(), 0.) / m_Nx;

	for (int i = 0; i < m_Nx; ++i)
		m_E.at(i) -= avg;
}
