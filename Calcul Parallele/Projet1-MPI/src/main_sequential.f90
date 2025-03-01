PROGRAM  MAIN_SEQ
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout => output_unit, stdin => input_unit, stderr => error_unit
    USE PARAMETRE
    USE COMMON
    USE CONSTANTES
    USE REQ_FUNCTION

    IMPLICIT NONE

    ! Matrice associé au valeur  sur la discretisation du domaine
    REAL(rp), DIMENSION(:, :), ALLOCATABLE :: U
    ! Permet de stocker les valeur de x et y sur la discretisation du domaine
    REAL(rp), DIMENSION(:), ALLOCATABLE :: Y, X

    ! Variable permettant de stocker des valeurs redondantes
    REAL(rp) :: a, up_y, down_k, kr

    INTEGER :: k, i, j

    ! Chargement des parametres stockés dans le module parametre
    CALL LOAD_PARAMETRE
    !    CALL PRINT_PARAMETRE

    ! Allocation de la grille de calcul
    ALLOCATE(U(Nx, Ny))

    ! Initialisation à 0
    DO j = 1, Ny
        DO i = 1, Nx
            U(i, j) = 0._rp
        END DO
    END DO

    ! Allocation des valeurs de discrétisation
    ALLOCATE(X(Nx), Y(Ny))
    CALL RANGE(L, Nx, X)
    CALL RANGE(B, Ny, Y)

    ! Calcul de la solution
    DO k = 1, Nk
        kr = REAL(k, rp)
        a = a_k_alpha(k, alpha)
        down_k = SINH(B * kr * PI / L)
        DO j = 1, Ny
            up_y = SINH((B - Y(j)) * kr * PI / L)
            DO i = 1, Nx
                U(i, j) = U(i, j) + a * up_y * SIN(kr * PI * X(i) / L) / down_k
            END DO
        END DO
    END DO

    ! Sauvegarde du résultat obtenu
    OPEN(unit = 42, file = filename, status = 'replace')
    DO i = 1, Nx
        WRITE(42, *) (U(i, j), j = 1, Ny)
    END DO

!    PRINT*, 'Valeur Max : ', MAXVAL(U)

END PROGRAM MAIN_SEQ