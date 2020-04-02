CMAKE_MINIMUM_REQUIRED(VERSION 3.15)
PROJECT("Milieux Poreux")

ENABLE_LANGUAGE(Fortran)
SET(CMAKE_INSTALL_PREFIX "../Lestests/")

ADD_EXECUTABLE(mainvf4
			   laplacienvfmacs.f90
			   init.f90
			   longr.f90
			   parmmage.f90
			   imprime.f90
			   intmatvec.f90
			   algebrelineaire.f90
			   plotvtkmod.f90
			   fsourcemod.f90
			   readmesh.f90
			   matrixinitVF4.f90
			   assemblevf4.f90
			   assembletheta.f90
			   ajout.f90)

INSTALL(TARGETS mainvf4
		RUNTIME DESTINATION .)