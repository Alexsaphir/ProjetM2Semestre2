#!/usr/bin/env python3

import f90nml
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


def main():
    nml: f90nml.namelist = f90nml.read('conf.nml')
    fileconv = nml['conv']['fileconv']
    V = np.genfromtxt(fileconv)
    plt.plot(V[:, 0], V[:, 1])
    plt.show()


if __name__ == '__main__':
    main()
