PROGRAM  MAIN_SEQ
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout => output_unit, stdin => input_unit, stderr => error_unit
    USE PARAMETRE
    USE COMMON
    USE REQ_FUNCTION

    IMPLICIT NONE

    REAL(rp), DIMENSION(:, :), ALLOCATABLE :: U
    REAL(rp), DIMENSION(:), ALLOCATABLE :: Y, X

    REAL(rp) :: a, up_y, down_k, kr

    INTEGER :: k, i, j
    ! Chargement des parametres stockés dans le module parametre
    CALL LOAD_PARAMETRE
    CALL PRINT_PARAMETRE
    !Allocation de la grille de calcul
    ALLOCATE(U(Nx, Ny))

    !Allocation des valeurs de discrétisation
    ALLOCATE(X(Nx), Y(Ny))
    CALL RANGE(L, Nx, X)
    CALL RANGE(B, Ny, Y)

    DO k = 0, Nk
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

END PROGRAM MAIN_SEQ