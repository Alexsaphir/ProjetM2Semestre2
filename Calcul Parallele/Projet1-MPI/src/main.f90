!https://www.scivision.dev/fortran-terminal-io/
!https://www.scivision.dev/eclipse-graphical-fortran-debug-photran-install/
!https://www.scivision.dev/debug-cmake-fortran-builds/
!https://www.scivision.dev/fortran-polymorphic-function-subroutine/
!https://www.scivision.dev/print-vs-write-fortran/
!https://www.scivision.dev/intel-mkl-lapack95-gfortran/
!https://www.scivision.dev/fortran-2008-submodule-cmake/
!https://www.scivision.dev/what-value-does-modern-fortran-add/
!https://www.scivision.dev/cmake-modern-fortran/
!https://www.scivision.dev/fortran-2018-coarray-quick-start/
!https://github.com/scivision/fortran2018-examples/tree/master/cxx

!https://github.com/scivision/fortran2018-examples

!https://isotc.iso.org/livelink/livelink?func=ll&objId=19441669&objAction=Open

! Permet de lire les parametre depuis un fichier de configuration
! https://github.com/scivision/fortran2018-examples/tree/master/namelist
PROGRAM Projet1
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout => output_unit, stdin => input_unit, stderr => error_unit
    USE MPI_F08
    USE MPI_COMMON
    USE PARAMETRE
    USE REQ_FUNCTION

    IMPLICIT NONE

    ! MPI stuff.
    INTEGER :: nprocs, rang, ierr

    REAL(rp), DIMENSION(:, :), ALLOCATABLE :: U, Ue
    REAL(rp), DIMENSION(:), ALLOCATABLE :: Y, X
    REAL(rp) :: a, up_y, down_k, kr
    INTEGER :: Nj, Nstart
    INTEGER :: k, i, j

    ! initialisation
    CALL MPI_INIT(ierr)
    !nombre processeurs
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
    !quel est mon rang
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang, ierr)

    IF(rang == 0) THEN
        PRINT*, 'Calcul de U sur la grille avec ', nprocs, 'processus'
    END IF
    ! Charge-les parametres en utilisant des namelistes
    CALL LOAD_PARAMETRE

    ! Alloue/init la grille de travail pour u de chaque processus
    ALLOCATE(U(Nx, Ny))
    IF(rang==0) THEN
        ALLOCATE(Ue(Nx, Ny))
        Ue = 0._rp
    END IF
    U = 0._rp
    ALLOCATE(X(Nx), Y(Ny))
    CALL RANGE(L, Nx, X)
    CALL RANGE(B, Ny, Y)

    ! Recupere la quantité de travaille
    Nj = GET_JOB_SIZE(nprocs, rang)
    Ns=1000
    Nstart = GET_JOB_START(nprocs, rang)
    PRINT*, 'Moi processus ', rang, ', je dois faire ', Nj, ' taches.'
    ! Chaque processus calcul sa partie
    ! L'utilisation de la forme {variable}_{indice} permet de représenter la dépandance de {variable}
    ! À l'indice {indice}

    DO k = Nstart, Nstart + Nj - 1, 1
        kr = REAL(k, rp)
        ! on calcule le coeff a_k^\alpha
        a = a_k_alpha(k, alpha)
        down_k = SINH(B * kr * PI / L)
        DO j = 1, Ny
            up_y = SINH((B - Y(j)) * kr * PI / L)
            DO i = 1, Nx
                U(i, j) = U(i, j) + a * up_y * SIN(kr * PI * X(i) / L) / down_k
            END DO
        END DO
    END DO
    CALL MPI_Barrier(MPI_COMM_WORLD)

    CALL MPI_Barrier(MPI_COMM_WORLD)
    ! Le processus 0 doit maintenant recevoir les données
    ! Pour ce faire on utilise une reduction sur les tableaux
    !    IF (rang==0) THEN
    !        CALL MPI_REDUCE(U, Ue, Nx * Ny, MPI_REAL16, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    !    ELSE
    !        CALL MPI_REDUCE(U, Ue, Nx * Ny, MPI_REAL16, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    !    END IF
    CALL MPI_REDUCE(ABS(U), Ue, Nx * Ny, MPI_REAL16, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    ! On sauvegarde alors
    IF(rang ==0) THEN
        OPEN(unit = 42, file = filename, status = 'replace')
        DO i = 1, Nx
            WRITE(42, *) (U(i, j), j = 1, Ny)
        END DO
        PRINT*, MAXVAL(Ue)
    END IF
    PRINT*, MAXVAL(ABS(U))
    CALL MPI_FINALIZE(ierr)

CONTAINS
END PROGRAM Projet1