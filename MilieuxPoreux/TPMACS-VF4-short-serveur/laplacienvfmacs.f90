PROGRAM laplacienVFmacs
    !---------------------------------------------------------------------
    !     * Programme resoud l'equation de laplace sur un domaine qcq
    !        par la methode
    !        ** VOLUMES Finis VF4 :
    !                       sch?ma ? deux points sur un maillage orthogonale
    !
    !       Sur le probl?me :
    !       - c div( grad u ) + theta * u div(Vu)= F    dans omega
    !                u=Gdb                      sur gamma
    !
    !----------------
    ! La m?thode utilise plusieurs types de maillages :
    !---------------------------------------------------------------------
    !========
    ! Modules
    !========
    USE longr
    USE parmmage
    USE imprime
    USE intmatvec
    USE algebrelineaire
    use plotvtkmod
    Use fsourcemod
    IMPLICIT NONE
    !==========================
    ! Declaration des tableaux
    !==========================
    TYPE(MatCreux) :: A
    !==================================
    ! Declaration des variables locales
    !==================================
    INTEGER :: jt, i, j, is, jv, kiter
    REAL(kind = long), DIMENSION(:), ALLOCATABLE :: U, Uexacte
    REAL(kind = long), DIMENSION(:), ALLOCATABLE :: VitesseX, VitesseY
    REAL(kind = long) :: tol, seuil

    !===================
    ! Debut du programme
    !===================
    prefix = 'LAPLAC  '
    !
    CALL init                    !* Initialisation
    print*, 'init ok '

    ! Lecture du maillage ? partir d'un fichier : MAILLAGEGEOx, x=1...6

    CALL readmesh
    print*, 'readmesh ok '

    !-----------------------------------------
    ! Allocation de la structure des donnees
    !-----------------------------------------
    CALL matrixinitVF4 (A)
    print*, 'matrix init ok'

    !-----------------------------------------
    ! Calcul du second membre F physique : A%F
    !-----------------------------------------
    A%F = 0.D0
    do i = 1, Nbt
        A%F (i) = fsource(???, ???, choixpb)
    enddo

    write(*, *)'scmem  ok'

    !-----------------------------------------------------------------
    ! Assemblage second membre du systeme linaire : A%Bg:= int_K A%F
    !----------------------------------------------------------------
    !
    A%Bg = A%F * AireK

    print*, 'rhs ok '


    !-------------------------------
    ! Assemblage de La matrice A
    !------------------------------

    !----------------- assemblage par segment---------
    call assembleVF4(A)      ! assemblage de -c D^2
    print*, 'assembleseg ok'

    call assembletheta(A)            ! Assemblage de theta*u
    print*, 'assembletheta ok'


    ! resloution du syst?me lin?aire  par la mathode du gradient conjugu?
    ALLOCATE(U(Nbt))
    tol = 5.d-10
    U = gradconj(A%Bg, A, tol)

    ! calcul de la solution exacte
    ALLOCATE(Uexacte(Nbt))
    DO i = 1, Nbt
        Uexacte(i) = gbord(CoordK(1, i), CoordK(2, i), ChoixPb)
    END DO

    !! ----------------------------
    !!  solution Comparaison
    !! ----------------------------

    WRITE(uprint, *)' U(x,y) , Uexact '

    WRITE(uprint, 200) (U(i), Uexacte(i), i = 1, size(U))

    WRITE(uprint, *)' Erreur infiny  : ', MAXVAL(ABS(U - Uexacte))
    WRITE(uprint, *)' Erreur infiny relative : ', MAXVAL(ABS(U - Uexacte)) / MAXVAL(ABS(Uexacte))
    WRITE(uprint, *)' Max solution exacte : ', MAXVAL(Uexacte)
    WRITE(uprint, *)' Max solution calculee : ', MAXVAL(U)
    WRITE(uprint, *)' Min solution exacte : ', MINVAL(Uexacte)
    WRITE(uprint, *)' Min solution calculee : ', MINVAL(U)
    !!
    WRITE(*, *)' Erreur infiny  : ', MAXVAL(ABS(U - Uexacte))
    WRITE(*, *)' Erreur infiny relative : ', MAXVAL(ABS(U - Uexacte)) / MAXVAL(ABS(Uexacte))
    WRITE(*, *)' Max solution exacte : ', MAXVAL(Uexacte)
    WRITE(*, *)' Max solution calculee : ', MAXVAL(U)
    WRITE(*, *)' Min solution exacte : ', MINVAL(Uexacte)
    WRITE(*, *)' Min solution calculee : ', MINVAL(U)



    ! Creation du fichier pour visit
    !--------------------------------

    CALL plot_vtk (U, 'Ucalcule', 'U')
    CALL plot_vtk (Uexacte, 'Uexacte', 'Uexacte')
    !!
    !

    100 FORMAT(10(E10.3, 2x))
    200 FORMAT(6(E14.6, 2x))
    CLOSE (uprint)
    PRINT*, 'fin du travail laplacien'
    print*, 'CHOIX PB = ', choixpb

END PROGRAM laplacienVFmacs





