# https://cliutils.gitlab.io/modern-cmake/

cmake_minimum_required(VERSION 3.15)
project("Méthodes semi-Lagrangiennes pour la simulation numérique des plasmas")


set(CMAKE_CXX_STANDARD 17)

add_executable(Projet src/main.cpp src/gridfd.h src/gridfd.cpp src/integrate.h)