#ifndef GRIDFD_H
#define GRIDFD_H

#include <vector>

typedef unsigned int uint;

class GridFD
{
public:
    GridFD();
    GridFD(double L, double Vmax, uint Nx, uint Nv);

    [[nodiscard]] double getL() const;
    [[nodiscard]] double getVmax() const;
    [[nodiscard]] uint getNx() const;
    [[nodiscard]] uint getNv() const;

private:
    double m_L{1.};
    double m_Vmax{10.};

    uint m_Nx{100};
    uint m_Nv{100};

    std::vector<std::vector<double>> m_Grid;
};

#endif // GRIDFD_H
