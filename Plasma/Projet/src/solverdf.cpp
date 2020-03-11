#include "solverdf.h"

solverDF::solverDF()
{
	
}

solverDF::solverDF(argSolver arg) : m_dt{arg.dt}, m_t{arg.t}, m_T{arg.T}, m_Grid(arg.grid) {}
