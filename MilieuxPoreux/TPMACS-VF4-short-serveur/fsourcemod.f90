Module fsourcemod

Contains

    !======================================================
    !=====================================================
    FUNCTION gbord(x, y, m)
        ! Calcul la solution exacte de l'equation
        !--------
        ! Modules
        !--------
        USE longr

        IMPLICIT NONE

        !--------------------------
        ! Declaration des arguments
        !--------------------------
        REAL(kind = long), INTENT(in) :: x, y
        integer :: m
        REAL(kind = long) :: gbord
        !----------------------------------
        ! Declaration des variables locales
        !----------------------------------
        CHARACTER(len = 6) :: oldprf
        REAL(kind = long) :: pi

        !-------------------
        ! Debut du programme
        !-------------------
        oldprf = prefix
        prefix = 'Gbord'

        !------
        ! Corps
        !------
        pi = 4.D0 * ATAN(1.D0)

        select case(m)
        case(1)
            gbord = 1.
        case(2)
            gbord = x + y
        case(3)
            gbord = x * x - y * y
        case(4)
            gbord = COS(5. * pi * (x + y))
        case(5)
            gbord = x * (1. - x) * y * (1. - y)
        case(6)
            gbord = SIN(pi * x) * SIN(pi * y)
        case default
            print*, ' pb gbord'
            stop
        end select
        !-----------------
        ! Fin du programme
        !-----------------
        prefix = oldprf

        RETURN
    END FUNCTION gbord


    FUNCTION fsource(x, y, m)
        !--------
        ! Modules
        !--------
        USE longr

        IMPLICIT NONE

        !--------------------------
        ! Declaration des arguments
        !--------------------------
        REAL(kind = long), INTENT(in) :: x, y
        Integer :: m
        REAL(kind = long) :: fsource
        !----------------------------------
        ! Declaration des variables locales
        !----------------------------------
        CHARACTER(len = 6) :: oldprf
        REAL(kind = long) :: pi
        REAL(kind = long) :: lea, leaa
        !-------------------
        ! Debut du programme
        !-------------------
        oldprf = prefix
        prefix = 'FSOUR'


        !------
        ! Corps
        !------
        pi = 4.D0 * ATAN(1.D0)
        Select case (m)
        case(1)
            fsource = theta
        case (2)
            fsource = theta * (x + y)
        case (3)
            fsource = (x * x - y * y) * theta
        case(4)
            fsource = (50. * pi * pi * Coef_diffusion + theta) * COS(5. * pi*(y + x))
        case(5)
            fsource = (1. - x) * x * (1. - y) * y * theta - Coef_diffusion * (-2. * (1. - y) * y - 2. * (1. - x) * x)
        case(6)
            fsource = SIN(pi*x)*SIN(pi*y)*theta+2.*pi*pi*Coef_diffusion*SIN(pi*x)*SIN(pi*y)
        end Select
        !----------------
        ! Fin du programme
        !-----------------
        prefix = oldprf

        RETURN
    END FUNCTION fsource


END Module fsourcemod
