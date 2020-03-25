#ifndef GRIDFD_H
#define GRIDFD_H

#include <ostream>
#include <vector>

typedef unsigned int uint;

class GridFD
{
public:
	GridFD();
	GridFD(double L, double Vmax, uint Nx, uint Nv);

	[[nodiscard]] double getL() const;
	[[nodiscard]] double getVmax() const;
	[[nodiscard]] uint	 getNx() const;
	[[nodiscard]] uint	 getNv() const;
	[[nodiscard]] double getDv() const;
	[[nodiscard]] double getDx() const;

	//    Pour un i donné retourne la valeur de x dans l'intervalle [0,L]
	[[nodiscard]] double getX(int i) const;
	// idem pour [-Vmax,Vmax]
	[[nodiscard]] double getV(int i) const;

	void init_f(double f0(double x, double v));

	[[nodiscard]] double  E(int p) const;
	[[nodiscard]] double& E(int p);

	[[nodiscard]] double  f(int p, int v) const;
	[[nodiscard]] double& f(int p, int v);

	[[nodiscard]] double  Rho(int p) const;
	[[nodiscard]] double& Rho(int p);

	void				 computeElectricField();
	[[nodiscard]] double maxElectricField() const;

	double electricEnergy() const;

	void print() const;
	void save(const std::string& filename) const;

private:
	void				computeElectricCharge();
	static unsigned int posMod(int a, unsigned int b)
	{
		// clang / gcc -O3 ne duplique pas les a%b
		// checked on the one and only https://godbolt.org/
		return (a % b > 0) ? a % b : (a % b + b) % b;
	}

private:
	double m_L {1.};
	double m_Vmax {10.};

	uint m_Nx {10};
	uint m_Nv {10};

	double dx {.1};
	double dv {2.};

	std::vector<std::vector<double>> m_f;	// distribution of the plasma
	std::vector<double>				 m_E;	// Electric field
	std::vector<double>				 m_rho; // Electric charge

	friend std::ostream& operator<<(std::ostream& os, const GridFD& G);
};

std::ostream& operator<<(std::ostream& os, const GridFD& G);

#endif // GRIDFD_H
