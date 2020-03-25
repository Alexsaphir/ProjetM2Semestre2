TEMPLATE = app
CONFIG += console c++17
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += src/main.cpp \
	src/gridfd.cpp \
	src/solverfd.cpp

HEADERS += \
	src/gridfd.h \
	src/integrate.h \
	src/solverfd.h
