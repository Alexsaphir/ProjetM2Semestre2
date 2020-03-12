MODULE PARAMETRE
    USE CONSTANTES
    USE MPI_F08
    USE, INTRINSIC :: ISO_FORTRAN_ENV
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

    ! PArametre pour l'analys de convergence de A
    INTEGER :: Kmax = 5000
    ! Parametre pour solution donné par l'auteur
    INTEGER :: solN = 1
    CHARACTER(30) :: fileconv

    NAMELIST /conf/ L, B, alpha, Ns, Nk, Nx, Ny, filename
    NAMELIST /articleSol/ solN
    NAMELIST /conv/ Kmax, fileconv
CONTAINS
    SUBROUTINE LOAD_PARAMETRE
        OPEN(newunit = u, file = fileconf, action = 'read')
        READ(u, nml = conf)
        READ(u, nml = articleSol)
        READ(u, nml = conv)

        CLOSE(u)
    END SUBROUTINE LOAD_PARAMETRE

    SUBROUTINE LOAD_PARAMETRE_MPI(rang)
        INTEGER :: rang

        IF (rang == 0)THEN
            CALL LOAD_PARAMETRE
        ENDIF

        ! On uitlise broadcast pour diffuser l'intégralité des valeurs
        CALL MPI_BCAST(L, 1,MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD)
        CALL MPI_BCAST(B, 1,MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD)
        CALL MPI_BCAST(alpha, 1,MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD)

        CALL MPI_BCAST(Ns, 1,MPI_INTEGER, 0, MPI_COMM_WORLD)
        CALL MPI_BCAST(Nk, 1,MPI_INTEGER, 0, MPI_COMM_WORLD)
        CALL MPI_BCAST(Nx, 1,MPI_INTEGER, 0, MPI_COMM_WORLD)
        CALL MPI_BCAST(Ny, 1,MPI_INTEGER, 0, MPI_COMM_WORLD)

        CALL MPI_BCAST(Kmax, 1,MPI_INTEGER, 0, MPI_COMM_WORLD)
        CALL MPI_BCAST(solN, 1,MPI_INTEGER, 0, MPI_COMM_WORLD)

    END SUBROUTINE LOAD_PARAMETRE_MPI

    SUBROUTINE PRINT_PARAMETRE()
        PRINT *, "Les dimensions physique du problèmes sont L=", L, ",B=", B, "."
        PRINT *, "Discretisation du domaine :              Nx=", Nx, "Ny=", Ny, "."
        PRINT *, "Limite de la série        : k=", Nk, "."
        PRINT *, "alpha =", alpha, "."
        PRINT *, "Point d'intégration Ns=", Ns, "."
        PRINT *, "Numero Probleme : ", solN
        PRINT *, "fileconf : ", fileconf
        PRINT *, "filename : ", filename
    END SUBROUTINE PRINT_PARAMETRE
END MODULE PARAMETRE