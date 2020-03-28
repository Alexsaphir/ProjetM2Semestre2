# https://cliutils.gitlab.io/modern-cmake/

CMAKE_MINIMUM_REQUIRED(VERSION 3.15)
PROJECT("Méthodes semi-Lagrangiennes pour la simulation numérique des plasmas")

SET(CMAKE_CXX_STANDARD 17)

SET(CMAKE_CXX_FLAGS "-O3")

IF (EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	INCLUDE(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
	CONAN_BASIC_SETUP()
ELSE ()
	MESSAGE(WARNING "The file conanbuildinfo.cmake doesn't exist, you have to run conan install first")
ENDIF ()

FIND_PACKAGE(OpenMP)
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
ADD_EXECUTABLE(Projet src/main.cpp src/grid.h src/grid.cpp src/solverfd.cpp src/solverfd.h src/solversl.cpp src/solversl.h src/clip.cpp )
TARGET_LINK_LIBRARIES(Projet OpenMP::OpenMP_CXX
					  ${CONAN_LIBS}
					  ${CONAN_LIBS})

ADD_EXECUTABLE(Convergence src/convergence.cpp src/grid.h src/grid.cpp src/solversl.cpp src/solversl.h)
TARGET_LINK_LIBRARIES(Convergence OpenMP::OpenMP_CXX
					  ${CONAN_LIBS}
					  ${CONAN_LIBS})

ADD_EXECUTABLE(Clip src/clip.cpp src/grid.h src/grid.cpp src/solversl.cpp src/solversl.h)
TARGET_LINK_LIBRARIES(Clip OpenMP::OpenMP_CXX
					  ${CONAN_LIBS}
					  ${CONAN_LIBS})

ADD_EXECUTABLE(KeenWave  src/grid.h src/grid.cpp src/solversl_keen.cpp src/solversl_keen.h src/keen_wave.cpp)
TARGET_LINK_LIBRARIES(KeenWave OpenMP::OpenMP_CXX
					  ${CONAN_LIBS}
					  ${CONAN_LIBS})