MODULE PARAMETRE
    USE CONSTANTES
    IMPLICIT NONE

    ! Parametre du probleme
    REAL(rp) :: L = 2._rp
    REAL(rp) :: B = 1._rp
    REAL(rp) :: alpha = 1._rp
    INTEGER :: Ns = 20 ! Nombre de sous intervalles pr l'intégrale
    INTEGER :: Nk = 47 ! Nombre de k utilisés dans la somme
    INTEGER :: Nx = 10, Ny = 10 ! Nombre de points ds la grille sur chaque direction
    CHARACTER(30) :: filename ! WARNING: Limite de caractère pour le nom du fichier de sortie

    CHARACTER(*), PARAMETER :: fileconf = 'conf.nml' ! Nom du fichier de configuration
    INTEGER, PRIVATE :: u

    namelist /conf/ L, B, alpha, Ns, Nk, Nx, Ny, filename

CONTAINS
    SUBROUTINE LOAD_PARAMETRE()
        OPEN(newunit = u, file = fileconf, action = 'read')
        READ(u, nml = conf)
        CLOSE(u)
    END SUBROUTINE LOAD_PARAMETRE
END MODULE PARAMETRE