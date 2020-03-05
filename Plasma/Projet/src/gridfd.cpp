#include "gridfd.h"

GridFD::GridFD()
{
    GridFD(m_L, m_Vmax, m_Nx, m_Nv); // Delegating constructor
}

GridFD::GridFD(double L, double Vmax, uint Nx, uint Nv) : m_L{L}, m_Vmax{Vmax}, m_Nx{Nx}, m_Nv{Nv}
{
    m_Grid.resize(Nx, std::vector<double>(Nv, 0.));
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
