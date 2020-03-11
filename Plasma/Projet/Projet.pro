TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += src/main.cpp \
    src/gridfd.cpp \
    src/solverdf.cpp

HEADERS += \
    src/gridfd.h \
    src/integrate.h \
    src/solverdf.h
