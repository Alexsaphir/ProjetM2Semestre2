#include "gridfd.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <numeric>

GridFD::GridFD()
{
	this->m_f.resize(m_Nx, std::vector<double>(m_Nv, 0.));
	this->m_E.resize(m_Nx, 0.);
	this->m_rho.resize(m_Nx, 0.);

	dx = m_L / static_cast<double>(m_Nx);
	dv = 2. * m_Vmax / static_cast<double>(m_Nv);
}

GridFD::GridFD(double L, double Vmax, uint Nx, uint Nv) : m_L {L}, m_Vmax {Vmax}, m_Nx {Nx}, m_Nv {Nv}
{
	this->m_f.resize(Nx, std::vector<double>(Nv + 1, 0.));
	this->m_E.resize(Nx, 0.);
	this->m_rho.resize(Nx, 0.);

	dx = L / static_cast<double>(Nx);
	dv = 2. * Vmax / static_cast<double>(Nv);
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

double GridFD::getDv() const
{
	return dv;
}

double GridFD::getDx() const
{
	return dx;
}

// Obtention
double GridFD::getX(int i) const
{
	return static_cast<double>(i) * dx;
}

double GridFD::getV(int i) const
{
	return static_cast<double>(i) * dv - m_Vmax;
}

double GridFD::E(int p) const
{
	return m_E.at(posMod(p , m_Nx));
}

double& GridFD::E(int p)
{
	return m_E.at(posMod(p , m_Nx));
}

double GridFD::f(int p, int v) const
{
	return m_f.at(posMod(p , m_Nx)).at(posMod(v , m_Nv));
}

double& GridFD::f(int p, int v)
{
	return m_f.at(posMod(p , m_Nx)).at(posMod(v , m_Nv));
}

double GridFD::Rho(int p) const
{
	return m_rho.at(posMod(p , m_Nx));
}

double& GridFD::Rho(int p)
{
	return m_rho.at(posMod(p , m_Nx));
}

double GridFD::maxElectricField()
{
	// Obtensu en regardant std::max(initialization_list)
	// https://en.cppreference.com/w/cpp/algorithm/max
	return *std::max_element(std::begin(m_E), std::end(m_E));
}

void GridFD::print() const
{
	std::cout << "-------";
	for (uint i = 0; i < m_Nx + 1; ++i)
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
	for (uint i = 0; i < m_Nx; ++i)
	{
		// Integration
		m_rho.at(i) = dv * std::reduce(m_f.at(i).begin(), m_f.at(i).end(), 0.);
	}
}

void GridFD::computeElectricField()
{
	computeElectricCharge();
	m_E.at(0) = 0.;
	for (uint i = 1; i < m_Nx; ++i)
	{
		m_E.at(i) = m_E.at(i - 1) + dx * (1. - m_rho.at(i));
	}

	// Calcul la moyenne
	double avg = dx * std::accumulate(m_E.begin(), m_E.end(), 0.) / m_L;

	// Offset
	// std::transform(std::begin(m_E),std::end(m_E),std::begin(m_E),[avg](int e){return e-avg;});
	for (uint i = 0; i < m_Nx; ++i)
		m_E.at(i) -= avg;
}

void GridFD::save(const std::string& filename) const
{
	std::ofstream out(filename, std::ofstream::out | std::ofstream::trunc);
	out << *this;
}

std::ostream& operator<<(std::ostream& os, const GridFD& G)
{
	for (uint i = 0; i < G.getNx() + 1; ++i)
	{
		double x = G.getX(i);
		for (uint j = 0; j < G.getNv() + 1; ++j)
		{
			double v = G.getV(j);
			os << x << ',' << v << ',' << G.f(i, j) << ',' << G.E(i) << '\n';
		}
	}
	return os;
}

double GridFD::electricEnergy() const
{
	// Encore une intégrale que l'on met sous une forme bizarre
	return std::sqrt(.5 * dx * std::inner_product(m_E.begin(), m_E.end(), m_E.begin(), 0.));
}
