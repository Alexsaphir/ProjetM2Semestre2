CMAKE_MINIMUM_REQUIRED(VERSION 3.15)
PROJECT(MPI_Projet1)

# Pour le CCIPL ça marche mieux avec ça
#SET(CMAKE_Fortran_COMPILER ifort)

ENABLE_LANGUAGE(Fortran)
FIND_PACKAGE(MPI REQUIRED COMPONENTS Fortran)

ADD_DEFINITIONS(${MPI_Fortran_COMPILE_FLAGS})
INCLUDE_DIRECTORIES(${MPI_Fortran_INCLUDE_DIRS})
LINK_DIRECTORIES(${MPI_FortranLIBRARY_DIRS})

#set (CMAKE_Fortran_FLAGS "-funroll-all-loops -O3 -ipo -xHost -xHost -g")

ADD_EXECUTABLE(projet_seq src/main_sequential.f90
        src/constantes.f90
        src/common.f90
        src/parametre.f90
        src/reqFunction.f90)
TARGET_LINK_LIBRARIES(projet_seq MPI::MPI_Fortran)

ADD_EXECUTABLE(projet_par_v0
        src/main_parallel_v0.f90
        src/constantes.f90
        src/parametre.f90
        src/common.f90
        src/reqFunction.f90)
TARGET_LINK_LIBRARIES(projet_par_v0 MPI::MPI_Fortran)

ADD_EXECUTABLE(projet_par_v1
			   src/main_parallel_v1.f90
			   src/constantes.f90
			   src/parametre.f90
			   src/common.f90
			   src/reqFunction.f90)
TARGET_LINK_LIBRARIES(projet_par_v1 MPI::MPI_Fortran)

ADD_EXECUTABLE(projet_convergence
        src/studyAconv.f90
        src/constantes.f90
        src/parametre.f90
        src/reqFunction.f90)
TARGET_LINK_LIBRARIES(projet_convergence MPI::MPI_Fortran)

ADD_CUSTOM_COMMAND(
        TARGET projet_seq POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        ${CMAKE_SOURCE_DIR}/conf/conf.nml
        $<TARGET_FILE_DIR:projet_seq>)

#ADD_CUSTOM_COMMAND(
#        TARGET ${PROJECT_NAME} POST_BUILD
#        COMMAND ${CMAKE_COMMAND} -E copy
#        ${CMAKE_SOURCE_DIR}/script/post-traitement.py
#        $<TARGET_FILE_DIR:${PROJECT_NAME}>)


MESSAGE("Compiler used : ${MPI_Fortran_COMPILER}")
MESSAGE("Source dir    : ${CMAKE_SOURCE_DIR}")
MESSAGE("Binary dir    : ${CMAKE_CURRENT_BINARY_DIR}")
MESSAGE("How to run    : ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} EXECUTABLE ${MPIEXEC_POSTFLAGS} ARGS")