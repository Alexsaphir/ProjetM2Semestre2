#include <iostream>

#include "gridfd.h"

int main() {
    std::cout << "Hello, World!" << std::endl;
    GridFD  G(1,1,3,3);
    G.print();

    return 0;
}
