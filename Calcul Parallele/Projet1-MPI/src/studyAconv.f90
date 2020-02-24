PROGRAM STUDY_A_CONV
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout => output_unit, stdin => input_unit, stderr => error_unit
    USE CONSTANTES
    USE REQ_FUNCTION
    IMPLICIT NONE

    INTEGER :: k

    CALL LOAD_PARAMETRE
    !On fixe Ns suffisament élévé
    Ns = 15000
    OPEN(unit=42, file = fileconv, status = 'replace')
    DO k=1, Kmax
        WRITE(42,*) k, a_k_alpha(k, alpha)
    END DO

END PROGRAM STUDY_A_CONV