#!/usr/bin/env python3

import f90nml
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D


def main():
    print('Post processing :')
    # Lecture du fichier conf.nml
    nml: f90nml.namelist = f90nml.read('conf.nml')

    # Chargement des paramètres
    L: float = nml['conf']['L']
    B: float = nml['conf']['B']
    alpha: float = nml['conf']['alpha']
    Ns: int = nml['conf']['Ns']
    Nk: int = nml['conf']['Nk']
    Nx: int = nml['conf']['Nx']
    Ny: int = nml['conf']['Ny']
    filename = nml['conf']['filename']

    x = np.linspace(0., L, Nx)
    y = np.linspace(0., B, Ny)

    data = np.genfromtxt(filename)

    # hf = plt.figure()
    # ha = hf.add_subplot(111, projection='3d')
    #
    # X, Y = np.meshgrid(x, y)  # `plot_surface` expects `x` and `y` data to be 2D
    # ha.plot_surface(X, Y, data)
    #
    plt.plot(x, data)
    plt.show()


if __name__ == '__main__':
    main()
