#include "gridfd.h"

#include <iostream>

GridFD::GridFD()
{
	this->m_f.resize(m_Nx, std::vector<double>(m_Nv, 0.));
}

GridFD::GridFD(double L, double Vmax, uint Nx, uint Nv) : m_L{L}, m_Vmax{Vmax}, m_Nx{Nx}, m_Nv{Nv}
{
	this->m_f.resize(m_Nx, std::vector<double>(m_Nv, 0.));
}

double GridFD::getL() const
{
	return m_L;
}

double GridFD::getVmax() const
{
	return m_Vmax;
}

uint GridFD::getNx() const
{
	return m_Nx;
}

uint GridFD::getNv() const
{
	return m_Nv;
}

double GridFD::getX(int i) const
{
	return static_cast<double>(i) * m_L * (1. / static_cast<double>(m_Nx));
}

double GridFD::getV(int i) const
{
	return static_cast<double>(i) * m_Vmax * (1. / static_cast<double>(m_Nv)) - m_Vmax;
}

double GridFD::E(int p) const
{
	return m_E.at(p % m_Nx);
}

double &GridFD::E(int p)
{
	return m_E.at(p % m_Nx);
}

double GridFD::f(int p, int v) const
{
	return m_f.at(p % m_Nx).at(v % m_Nv);
}

double &GridFD::f(int p, int v)
{
	return m_f.at(p % m_Nx).at(v % m_Nv);
}

double GridFD::Rho(int p) const
{
	return m_rho.at(p % m_Nx);
}

double &GridFD::Rho(int p)
{
	return m_rho.at(p % m_Nx);
}

void GridFD::print() const
{
	for (auto V : m_f)
	{
		for (auto d : V)
		{
			std::cout << d << ' ';
		}
		std::cout << '\n';
	}
}
