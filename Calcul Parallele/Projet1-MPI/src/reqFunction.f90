MODULE REQ_FUNCTION
    USE CONSTANTES
    USE PARAMETRE
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
CONTAINS
    FUNCTION h(xi)
        REAL(rp), INTENT(IN) :: xi
        REAL(rp) :: h

        REAL(rp) :: solNr
        solNr = REAL(solN, rp)

        h = SIN(solNr * xi) / solNr
    END FUNCTION h

    FUNCTION hsk(xi, k)
        REAL(rp), INTENT(IN) :: xi
        INTEGER, INTENT(IN) :: k

        REAL(rp) :: hsk

        hsk = SIN((REAL(k, rp) * PI * xi) / L) * h(xi)
    END FUNCTION hsk

    ! Permet de calculer l'intégrale associée au problème en utilisant simpson composite.
    FUNCTION INTEGRAL_PB(k)
        INTEGER, INTENT(IN) :: k !
        REAL(rp) :: INTEGRAL_PB
        REAL(rp) :: s, dx, xi
        INTEGER :: i

        dx = L / REAL(Ns, rp)
        xi = 0._rp
        s = 0._rp
        DO i = 1, Ns
            !s = s+ dx * hsk(xi + .5_rp * dx, k)
            s = s + dx*(hsk(xi, k) + 4._rp * hsk(xi + .5_rp * dx, k) + hsk(xi + dx, k)) / 6._rp
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
END MODULE REQ_FUNCTION