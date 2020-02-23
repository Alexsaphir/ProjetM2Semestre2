PROGRAM MPI_Communication
    IMPLICIT none
    INCLUDE 'mpif.h'

    INTEGER :: nprocs, rang, ierr
    INTEGER :: valeur, code

    INTEGER, DIMENSION(:), ALLOCATABLE :: V
    REAL, DIMENSION(:), ALLOCATABLE :: U
    REAL, DIMENSION(:), ALLOCATABLE :: W
    INTEGER, DIMENSION(:), ALLOCATABLE :: index
    INTEGER :: N, i

    REAL :: sum_partial


    ! initialisation
    CALL MPI_INIT(ierr)

    !nombre processeurs
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
    !quel est mon rang
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang, ierr)

    WRITE(*, *) "MPI Communication : ", GET_SIZE_TASK(2, nprocs, rang)

    IF (rang == 2) THEN
        WRITE(*, *) "Envoi de la valeur aux  ", nprocs, " processeurs. "
        valeur = 40. + rang
    END IF

    CALL MPI_BCAST (valeur, 1, MPI_INTEGER, 2, MPI_COMM_WORLD, code)

    WRITE(*, *) "Moi, processus ", rang, ", j'ai reçu ", valeur, " du processus 2"

    IF(rang==0) THEN
        WRITE(*, *) "Allocation du tableau..."
        ALLOCATE(V(nprocs))
        V = -1
    END IF

    ! Modification de valeur par chaque processeur
    valeur = valeur + rang

    ! GATHER de valeur de chaque procs dans le tableau V
    ! Valeur pr le procs recevant
    ! Si on utilise le inplace on suppose que la variable est déja placé dans le tableau
    IF(rang ==  0) THEN
        CALL MPI_GATHER(MPI_IN_PLACE, & ! Valeur pr le procs recevant
                1, & ! Nombre de variable a recevoir s_count
                MPI_INTEGER, & ! Type de variable s_type
                V, & !Adresse de reception
                1, & !r_count = s_count
                MPI_INTEGER, & ! r_type
                0, & ! Processus root qui recoit les données
                MPI_COMM_WORLD, code)
    ELSE
        CALL MPI_GATHER(valeur, & ! Valeur pr le procs recevant
                1, & ! Nombre de variable a recevoir s_count
                MPI_INTEGER, & ! Type de variable s_type
                V, & !Adresse de reception
                1, & !r_count = s_count
                MPI_INTEGER, & ! r_type
                0, & ! Processus root qui recoit les données
                MPI_COMM_WORLD, code)
    END IF

    IF (rang ==0) THEN
        WRITE(*, *) "Resultat :", V
    END IF

    CALL MPI_BARRIER(MPI_COMM_WORLD, code)
    IF(rang == 0) THEN
        WRITE(*, *) "Utilisons maintenant des tableaux pour GATHER comme variable envoyés."
        ALLOCATE(index(nprocs))
        DO i = 1, nprocs
            index(i) = MOD(IRAND(), 20)
        END DO
    END IF
    ! scatter the array to all process
    CALL MPI_SCATTER(index, & ! addresse du tableau a scatter
            1, & ! Taille du paquet envoyés
            MPI_INTEGER, &

            N, &
            1,&
            MPI_INTEGER, &

            0, & ! Processus root qui envoit les données
            MPI_COMM_WORLD, code)

    ! Chaque proc peut maintenant allouer son tableau
    ALLOCATE(U(N))
    DO i = 1,N
        U(i) = RAND()*10.
    END DO

    WRITE(*, *) "    >", rang, ",", N

    CALL MPI_FINALIZE(ierr)
CONTAINS
    FUNCTION GET_SIZE_TASK(N, nprocs, proc)
        INTEGER, INTENT(IN) :: N, nprocs, proc
        INTEGER :: GET_SIZE_TASK

        GET_SIZE_TASK = N / nprocs

        IF(proc < MOD(N, nprocs)) THEN
            GET_SIZE_TASK = GET_SIZE_TASK + 1
        END IF
    END FUNCTION GET_SIZE_TASK

END PROGRAM MPI_Communication

