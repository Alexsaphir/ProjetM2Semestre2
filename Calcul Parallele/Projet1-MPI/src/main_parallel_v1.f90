! https://tech.io/playgrounds/349/introduction-to-mpi/introduction-to-distributed-computing

PROGRAM MAIN_PARALLEL
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout => output_unit, stdin => input_unit, stderr => error_unit
    USE MPI_F08
    USE CONSTANTES
    USE COMMON
    USE PARAMETRE
    USE REQ_FUNCTION

    IMPLICIT NONE

    ! MPI stuff.
    INTEGER :: nprocs, rang, ierr

    REAL(rp), DIMENSION(:, :), ALLOCATABLE :: U, U_final
    REAL(rp), DIMENSION(:), ALLOCATABLE :: Y, X
    REAL(rp) :: a, up_y, down_k, kr
    REAL(dp) :: t0, t1
    INTEGER :: Nj, Nstart, N
    INTEGER :: k, i, j, Nik
    REAL(rp), DIMENSION(:), ALLOCATABLE :: Va

    ! initialisation et obtention valeur rang, nprocs
    CALL MPI_INIT(ierr)
    CALL FILL_MPI_VALUE(nprocs, rang, ierr)

    IF(rang == 0) THEN
        CALL LOAD_PARAMETRE_MPI(rang)
!        PRINT*, 'Résolution du problème avec ', nprocs, 'processus'
    END IF
    ! Charge-les parametres en utilisant des namelistes
    ! Pour éviter de surcharger le disque en io sur un fichier le rang 0 charge les valeurs et les propages
    CALL LOAD_PARAMETRE_MPI(rang)
    CALL MPI_Barrier(MPI_COMM_WORLD)

    ! Alloue/init la grille de travail et les vecteur position de chaque processus
    ALLOCATE(U(Nx, Ny))
    ALLOCATE(X(Nx), Y(Ny))

    IF(rang==0) THEN
        ALLOCATE(U_final(Nx, Ny))
        U_final = 0._rp
    END IF
    U = 0._rp
    CALL RANGE(L, Nx, X)
    CALL RANGE(B, Ny, Y)

    ! Chaque processus calcul sa partie
    ! L'utilisation de la forme {variable}_{indice} permet de représenter la dépandance de {variable}
    ! À l'indice {indice}
    ! De plus, pour un indice entier, on nomme sa version real sous la forme suivante {variable}r

    ! Calcul des différentes valeurs de a_k_alpha
    ! le temps d'éxécution de cette boucle est suffisament faible
    ! pour réduire le temps d'éxécution cumulés, on propose que Nk noeud calcule une valeur
    ! puis la propage a tous les autres noeuds
    ! Recupere la quantité de travaille
    ALLOCATE(Va(Nk))
    Va = 0._rp

    Nj = GET_JOB_SIZE(nprocs, rang, Nk)
    Nstart = GET_JOB_START(nprocs, rang, Nk)

    DO k = Nstart, Nstart + Nj - 1, 1
        kr = REAL(k, rp)
        ! on calcule le coeff a_k^\alpha
        Va(k) = a_k_alpha(k, alpha)
    END DO
    CALL MPI_ALLREDUCE(MPI_IN_PLACE, Va, Nk, MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD)
    ! On attends que tous les noeuds ait bien reçu les éléments.
    CALL MPI_Barrier(MPI_COMM_WORLD)

    t0 = MPI_Wtime()
    ! Recupere la quantité de travaille
    Nj = GET_JOB_SIZE(nprocs, rang, Nx * Ny)
    Nstart = GET_JOB_START(nprocs, rang, Nx * Ny)

    ! On fait une boucle sur les indices i,j et k de maniere lineaire
    DO k=1,Nk
    DO N = Nstart, Nstart + Nj - 1, 1
        ! On calcule i j et k a partir de la position lineaire
!        CALL LINEARTO2D(N, Nk, Nx * Ny, k, Nik)
!        CALL LINEARTO2D(Nik, Nx, Ny, i, j)
        CALL LINEARTO2D(N, Ny,Nx, j,i)

        kr = REAL(k, rp)
        down_k = SINH(B * kr * PI / L)
        up_y = SINH((B - Y(j)) * kr * PI / L)
        U(i, j) = U(i, j) + Va(k) * up_y * SIN(kr * PI * X(i) / L) / down_k
    END DO
    END DO

    CALL MPI_Barrier(MPI_COMM_WORLD)

    ! Le processus 0 doit maintenant recevoir les données
    ! Pour ce faire on utilise une reduction sur les tableaux
    IF (rang==0) THEN
        CALL MPI_REDUCE(U, U_final, Nx * Ny, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    ELSE
        CALL MPI_REDUCE(U, U_final, Nx * Ny, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    END IF
    CALL MPI_Barrier(MPI_COMM_WORLD)
    t1 = MPI_Wtime()
    ! On sauvegarde alors

    IF(rang ==0) THEN
        OPEN(unit = 42, file = filename, status = 'replace')
        DO i = 1, Nx
            WRITE(42, *) (U_final(i, j), j = 1, Ny)
        END DO
!        PRINT*, 'Elapsed : ', t1 - t0
!        PRINT*, 'Valeur Max : ', MAXVAL(U_final)
    END IF

    CALL MPI_FINALIZE(ierr)
END PROGRAM MAIN_PARALLEL