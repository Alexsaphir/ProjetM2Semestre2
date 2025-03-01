MODULE COMMON
    USE CONSTANTES
    USE PARAMETRE
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
CONTAINS
    SUBROUTINE FILL_MPI_VALUE(nprocs, rang, ierr)
        INTEGER, INTENT(INOUT) :: nprocs, rang, ierr
        !nombre processeurs
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
        !quel est mon rang
        CALL MPI_COMM_RANK(MPI_COMM_WORLD, rang, ierr)
    END SUBROUTINE FILL_MPI_VALUE

    FUNCTION GET_JOB_START(nprocs, rang, N)
        INTEGER, INTENT(IN) :: nprocs
        INTEGER, INTENT(IN) :: rang
        INTEGER, INTENT(IN) :: N

        INTEGER :: GET_JOB_START

        INTEGER :: N_TASK_SUP, count

        ! On est sur que chaque processus a au moins fait le minimum requis
        count = N / nprocs
        ! on doit maintenant savoir le nombre de processus qui ont du faire un calcul de plus
        ! on sait que l'on a MODULO(Nk, nprocs) tache restante
        N_TASK_SUP = MODULO(N, nprocs)

        IF(rang<N_TASK_SUP)THEN
            GET_JOB_START = (count+1) * rang +1
        ELSE
            ! on sait que toutes les taches sup ont été traités par les processus inférieur
            GET_JOB_START = rang * count + N_TASK_SUP + 1
        END IF

    END FUNCTION GET_JOB_START

    FUNCTION GET_JOB_SIZE(nprocs, rang, N)
        INTEGER, INTENT(IN) :: nprocs
        INTEGER, INTENT(IN) :: rang
        INTEGER, INTENT(IN) :: N

        INTEGER :: GET_JOB_SIZE

        GET_JOB_SIZE = N / nprocs
        IF(rang < MODULO(N, nprocs)) THEN
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

    SUBROUTINE LINEARTO2D(N, Ni, Nj, i, j)
        INTEGER, INTENT(IN) :: N, Ni, Nj
        INTEGER, INTENT(OUT) :: i, j

        j = (N - 1) / Ni + 1
        i = MOD(N - 1, Ni) + 1
    END SUBROUTINE

    FUNCTION ARRAY3DTOLINEAR(i, j, k, Ni, Nj, Nk)
        INTEGER, INTENT(IN) :: Nk, Ni, Nj
        INTEGER, INTENT(IN) :: i, j, k
        INTEGER :: ARRAY3DTOLINEAR

        ARRAY3DTOLINEAR = k + (i - 1) * Nk + (j - 1) * Ni
    END FUNCTION ARRAY3DTOLINEAR
END MODULE COMMON