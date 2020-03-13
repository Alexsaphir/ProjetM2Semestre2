#include "solverfd.h"

solverDF::solverDF(double dt, double T, double L, double Vmax, uint Nx, uint Nv) : m_dt(dt), m_t(0.), m_T(T), m_Grid(L, Vmax, Nx, Nv)
{
}

