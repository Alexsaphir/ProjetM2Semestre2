MODULE PARAMETRE
    USE CONSTANTES
    IMPLICIT NONE

    ! Parametre du probleme
    REAL(rp), PARAMETER :: L = 2.
    REAL(rp), PARAMETER :: B = 1.
    REAL(rp), PARAMETER :: alpha = 1.
    INTEGER, PARAMETER :: Ns = 20 ! Nombre de sous intervalles
    INTEGER, PARAMETER :: Nk = 47 ! Nombre de k utilisés dans la somme
    INTEGER, PARAMETER :: Nx = 10, Ny = 10 ! Nombre de point ds la grille sur chaque direction
    CHARACTER(*), PARAMETER :: filename = 'data.out'
END MODULE PARAMETRE