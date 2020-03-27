#include "grid.h"

#include <algorithm>
#include <cmath>
#include <complex.h>
#include <fftw3.h>
#include <fstream>
#include <iostream>
#include <numeric>

Grid::Grid()
{
	this->m_f.resize(m_Nx, std::vector<double>(m_Nv, 0.));
	this->m_E.resize(m_Nx + 1, 0.);
	this->m_rho.resize(m_Nx + 1, 0.);
	this->m_out.resize(m_Nx + 1);

	dx = m_L / static_cast<double>(m_Nx);
	dv = 2. * m_Vmax / static_cast<double>(m_Nv);
}

Grid::Grid(double L, double Vmax, uint Nx, uint Nv) : m_L {L}, m_Vmax {Vmax}, m_Nx {Nx}, m_Nv {Nv}
{
	this->m_f.resize(Nx, std::vector<double>(Nv + 1, 0.));
	this->m_E.resize(Nx + 1, 0.);
	this->m_rho.resize(Nx + 1, 0.);
	this->m_out.resize(Nx + 1);

	dx = L / static_cast<double>(Nx);
	dv = 2. * Vmax / static_cast<double>(Nv);
}

// Obtention des dimensions du domaine
double Grid::getL() const
{
	return m_L;
}

double Grid::getVmax() const
{
	return m_Vmax;
}

// Obtention des dimension du maillage
uint Grid::getNx() const
{
	return m_Nx;
}

uint Grid::getNv() const
{
	return m_Nv;
}

double Grid::getDv() const
{
	return dv;
}

double Grid::getDx() const
{
	return dx;
}

// Obtention
double Grid::getX(int i) const
{
	return static_cast<double>(i) * dx;
}

double Grid::getV(int i) const
{
	return static_cast<double>(i) * dv - m_Vmax;
}

double Grid::E(int p) const
{
	return m_E.at(posMod(p, m_Nx));
}

double& Grid::E(int p)
{
	return m_E.at(posMod(p, m_Nx));
}

double Grid::f(int p, int v) const
{
	return m_f.at(posMod(p, m_Nx)).at(posMod(v, m_Nv));
}

double& Grid::f(int p, int v)
{
	return m_f.at(posMod(p, m_Nx)).at(posMod(v, m_Nv));
}

double Grid::Rho(int p) const
{
	return m_rho.at(posMod(p, m_Nx));
}

double& Grid::Rho(int p)
{
	return m_rho.at(posMod(p, m_Nx));
}

double Grid::maxElectricField() const
{
	// Obtenu en regardant std::max(initialization_list)
	// https://en.cppreference.com/w/cpp/algorithm/max
	return *std::max_element(std::begin(m_E), std::end(m_E));
}

void Grid::print() const
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

void Grid::init_f(double f0(double , double ))
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

void Grid::computeElectricCharge()
{
	for (uint i = 0; i < m_Nx; ++i)
	{
		// Integration
		m_rho.at(i) = dv * std::accumulate(m_f.at(i).begin(), m_f.at(i).end(), 0.);
	}
	m_rho.at(m_Nx) = m_rho.at(0);
}

void Grid::computeElectricField()
{
	// Commun
	computeElectricCharge();

	// USE Fourrier
	int		  N = m_Nx + 1;
	fftw_plan toFourrier, fromFourrier;
	toFourrier	 = fftw_plan_dft_r2c_1d(N, m_rho.data(), reinterpret_cast<fftw_complex*>(m_out.data()),
										FFTW_ESTIMATE);
	fromFourrier = fftw_plan_dft_c2r_1d(N, reinterpret_cast<fftw_complex*>(m_out.data()), m_E.data(),
										FFTW_ESTIMATE);

	fftw_execute(toFourrier);
	// on a alors les coeff de rho ds fourrier
	for (int k = -N / 2; k < N / 2; ++k)
	{
		if (k == 0)
		{
			m_out.at(k + N / 2) *= 0.;
			continue;
		}
		std::complex<double> tmp = -I * m_L / (2. * M_PI * k);
		m_out.at(k + N / 2) *= tmp;
	}
	m_out.at(0) = 0.;
	fftw_execute(fromFourrier);
	return;


	// Pas fourrier


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

void Grid::save(const std::string& filename) const
{
	std::ofstream out(filename, std::ofstream::out | std::ofstream::trunc);
	out << *this;
}

std::ostream& operator<<(std::ostream& os, const Grid& G)
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

double Grid::electricEnergy() const
{
	// Encore une intégrale que l'on met sous une forme bizarre
	return std::sqrt(.5 * dx * std::inner_product(m_E.begin(), m_E.end(), m_E.begin(), 0.));
}
