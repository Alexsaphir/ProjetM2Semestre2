# https://cliutils.gitlab.io/modern-cmake/

cmake_minimum_required(VERSION 3.15)
project("Méthodes semi-Lagrangiennes pour la simulation numérique des plasmas")

set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS "-O3")

if(EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	conan_basic_setup()
else()
	message(WARNING "The file conanbuildinfo.cmake doesn't exist, you have to run conan install first")
endif()

find_package(OpenMP)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
add_executable(Projet src/main.cpp src/grid.h src/grid.cpp src/integrate.h src/solverfd.cpp src/solverfd.h src/solversl.cpp src/solversl.h)
target_link_libraries(Projet OpenMP::OpenMP_CXX
					  ${CONAN_LIBS}
					  ${CONAN_LIBS})