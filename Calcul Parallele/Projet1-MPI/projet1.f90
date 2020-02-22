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
PROGRAM Projet1
    USE MPI_F08
    IMPLICIT NONE

    INTEGER, PARAMETER :: rp = 16
    REAL(rp), PARAMETER :: pi = ACOS(0._rp)
    REAL(rp), PARAMETER :: l = 1.
    REAL(rp), PARAMETER :: b = 1.

    INTEGER :: nprocs, rang, ierr

    ! initialisation
    CALL MPI_INIT(ierr)
    !nombre processeurs
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
    !quel est mon rang
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang, ierr)

    WRITE(*, *) INTEGRAL_PB(10._rp, 3)

    CALL MPI_FINALIZE(ierr)
CONTAINS
    FUNCTION h(xi)
        REAL(rp), INTENT(IN) :: xi
        REAL(rp) :: h
        h = 2
        h = 1._rp
    END FUNCTION h

    FUNCTION hsk(xi, k)
        REAL(rp), INTENT(IN) :: xi
        REAL(rp), INTENT(IN) :: k

        REAL(rp) :: hsk

        hsk = SIN(b * k * xi / l) * H(xi)
    END FUNCTION hsk

    ! Permet de calculer l'intégrale associés au problème en utilisant simpson composite.
    FUNCTION INTEGRAL_PB(k, Ns)
        REAL(rp), INTENT(IN) :: k !
        INTEGER, INTENT(IN) :: Ns ! Parametre pour le nombre de segment a utiliser pr l'integration composite
        REAL(rp) :: INTEGRAL_PB
        REAL(rp) :: s, dx, xi
        INTEGER :: i

        dx = l / REAL(Ns, rp)
        xi = 0._rp
        s = 0._rp
        DO i = 1, Ns
            s = s + dx * (hsk(xi, k) + 4._rp * hsk(xi + .5_rp * dx, k) + hsk(xi + dx, k)) / 6._rp
            xi = xi + dx
        END DO
        INTEGRAL_PB = s
    END FUNCTION INTEGRAL_PB

END PROGRAM Projet1