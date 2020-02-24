MODULE MPI_COMMON
    USE CONSTANTES
    USE PARAMETRE
    IMPLICIT NONE
CONTAINS
    FUNCTION GET_JOB_START(nprocs, rang)
        INTEGER, INTENT(IN) :: nprocs
        INTEGER, INTENT(IN) :: rang
        INTEGER :: GET_JOB_START

        INTEGER :: N_TASK_SUP

        ! On est sur que chaque processus a au moins fait le minimum requis
        GET_JOB_START = rang * Nk / nprocs
        ! on doit maintenant savoir le nombre de processus qui ont du faire un calcul de plus
        ! on sait que l'on a MODULO(Nk, nprocs) tache restante
        N_TASK_SUP = MODULO(Nk, nprocs)

        IF(rang<N_TASK_SUP)THEN
            GET_JOB_START = GET_JOB_START + rang + 1
        ELSE
            ! on sait que toutes les taches sup ont été traités par les processus inférieur
            GET_JOB_START = GET_JOB_START + N_TASK_SUP + 1
        END IF

    END FUNCTION GET_JOB_START

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
END MODULE MPI_COMMON