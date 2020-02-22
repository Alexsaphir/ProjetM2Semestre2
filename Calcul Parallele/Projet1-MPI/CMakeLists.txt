cmake_minimum_required(VERSION 3.9.6)
project(MPI_Communication Fortran)
enable_language(Fortran)
find_package(MPI REQUIRED COMPONENTS Fortran)

# set(CMAKE_Fortran_COMPILER mpif90)

add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_DIRS})
link_directories(${MPI_FortranLIBRARY_DIRS})

add_executable(MPI_derivedType derived_type.f90)
target_link_libraries(MPI_derivedType MPI::MPI_Fortran)

add_executable(MPI_Communication main.f90)
target_link_libraries(MPI_Communication MPI::MPI_Fortran)

add_executable(MPI_Projet1 projet1.f90)
target_link_libraries(MPI_Projet1 MPI::MPI_Fortran)

MESSAGE(STATUS "Compiler used : ${MPI_Fortran_COMPILER}")
message(STATUS "Run: ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} EXECUTABLE ${MPIEXEC_POSTFLAGS} ARGS")
