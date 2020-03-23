CMAKE_MINIMUM_REQUIRED(VERSION 3.15)
PROJECT("Milieux Poreux")

ENABLE_LANGUAGE(Fortran)

ADD_EXECUTABLE(VF4
			   laplacienvfmacs.f90
			   longr.f90
			   parmmage.f90
			   imprime.f90
			   intmatvec.f90
			   algebrelineaire.f90
			   plotvtkmod.f90
			   fsourcemod.f90)
