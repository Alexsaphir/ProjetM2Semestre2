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

!https://github.com/scivision/fortran2018-examples

!https://isotc.iso.org/livelink/livelink?func=ll&objId=19441669&objAction=Open

! Permet de lire les parametre depuis un fichier de configuration
! https://github.com/scivision/fortran2018-examples/tree/master/namelist
PROGRAM Projet1
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout => output_unit, stdin => input_unit, stderr => error_unit
    USE MPI_F08
    IMPLICIT NONE

    INTEGER, PARAMETER :: rp = 16
    REAL(rp), PARAMETER :: pi = 2._rp * ACOS(0._rp)

    ! Parametre du probleme
    REAL(rp), PARAMETER :: L = 2.
    REAL(rp), PARAMETER :: B = 1.
    REAL(rp), PARAMETER :: alpha = 1.
    INTEGER, PARAMETER :: Ns = 20 ! Nombre de sous intervalles
    INTEGER, PARAMETER :: Nk = 47 ! Nombre de k utilisés dans la somme
    INTEGER, PARAMETER :: Nx = 10, Ny = 10 ! Nombre de point ds la grille sur chaque direction
    ! MPI stuff.
    INTEGER :: nprocs, rang, ierr

    REAL(rp), DIMENSION(:, :), ALLOCATABLE :: U
    REAL(rp), DIMENSION(:), ALLOCATABLE :: Y, X
    REAL(rp) :: a, up_y, down_k, kr
    INTEGER :: Nj
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
    ! Alloue/init la grille de travail pour u de chaque processus
    ALLOCATE(U(Nx, Ny))
    U = 0._rp
    ALLOCATE(X(Nx), Y(Ny))
    CALL RANGE(L, Nx, X)
    CALL RANGE(B, Ny, Y)

    ! Recupere la quantité de travaille
    Nj = GET_JOB_SIZE(nprocs, rang)
    PRINT*, 'Moi processus ', rang, ', je dois faire ', Nj, ' taches.'

    ! Chaque processus calcul sa partie
    ! l'utilisation de la forme {variable}_{indice} permet de représenter la dépandance de {variable}
    ! à l'indice {indice}
    DO k = rang + 1, rang + Nj + 1
        kr = REAL(k, rp)
        ! on calcule le coeff a_k^\alpha
        a = a_k_alpha(k, alpha)
        down_k = SINH(B * kr * PI / L)
        DO j = 1, Ny
            up_y = SINH((B - Y(j)) * kr * PI / L)
            DO i = 1, Nx
                U(i, j) = a * up_y * SIN(kr * PI * X(i) / L) / down_k
            END DO
        END DO
    END DO

    ! Le processus 0 doit maintenant recevoir les données
    ! Pour ce faire on utilise une reduction sur les tableaux
    IF (rang==0) THEN
        !
        CALL MPI_REDUCE(MPI_IN_PLACE, U, Nx*Ny, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    ELSE
        CALL MPI_REDUCE(U, U, Nx*Ny, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    END IF

    ! On sauvegarde alors

    CALL MPI_FINALIZE(ierr)
CONTAINS
    FUNCTION h(xi)
        REAL(rp), INTENT(IN) :: xi
        REAL(rp) :: h
        h = 1._rp
    END FUNCTION h

    FUNCTION hsk(xi, k)
        REAL(rp), INTENT(IN) :: xi
        INTEGER, INTENT(IN) :: k

        REAL(rp) :: hsk

        hsk = SIN((REAL(k, rp) * PI * xi) / L) * h(xi)
    END FUNCTION hsk

    ! Permet de calculer l'intégrale associés au problème en utilisant simpson composite.
    FUNCTION INTEGRAL_PB(k)
        INTEGER, INTENT(IN) :: k !
        REAL(rp) :: INTEGRAL_PB
        REAL(rp) :: s, dx, xi
        INTEGER :: i

        dx = L / REAL(Ns, rp)
        xi = 0._rp
        s = 0._rp
        DO i = 1, Ns
            s = s + dx * (hsk(xi, k) + 4._rp * hsk(xi + .5_rp * dx, k) + hsk(xi + dx, k)) / 6._rp
            xi = xi + dx
        END DO
        INTEGRAL_PB = s
    END FUNCTION INTEGRAL_PB

    FUNCTION a_k_alpha(k, alpha)
        INTEGER, INTENT(IN) :: k
        REAL(rp), INTENT(IN) :: alpha
        REAL(rp) :: a_k_alpha
        REAL(rp) :: up, down, kr

        kr = REAL(k, rp)
        up = 2._rp * SINH((B * kr * PI) / L)
        down = alpha * L * SINH((B * kr * PI) / L) + kr * PI

        a_k_alpha = up * INTEGRAL_PB(k) / down
    END FUNCTION a_k_alpha

    FUNCTION GET_JOB_SIZE(nprocs, rang)
        INTEGER, INTENT(IN) :: nprocs
        INTEGER, INTENT(IN) :: rang

        INTEGER :: GET_JOB_SIZE

        GET_JOB_SIZE = Nk / nprocs
        IF(rang < MODULO(Nk, nprocs)) THEN
            GET_JOB_SIZE = GET_JOB_SIZE + 1
        END IF
    END FUNCTION GET_JOB_SIZE

    SUBROUTINE RANGE(length, N, X)
        REAL(rp), INTENT(IN) :: length
        INTEGER, INTENT(IN) :: N
        REAL(rp), INTENT(INOUT), DIMENSION(N) :: X

        INTEGER :: i
        REAL(rp) :: ds
        ds = length / REAL(N - 1, rp)
        X(1) = 0._rp
        DO i = 2, N
            X(i) = X(i - 1) + ds
        END DO
    END SUBROUTINE RANGE
END PROGRAM Projet1