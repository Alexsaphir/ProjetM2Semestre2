PROGRAM MPI_Communication
    IMPLICIT none
    INCLUDE 'mpif.h'

    TYPE POINT
        REAL(8) x
        REAL(8) y
    END TYPE POINT

    INTEGER :: nprocs, rang, ierr
    INTEGER :: valeur, code
    TYPE(POINT) P

    ! initialisation
    CALL MPI_INIT(ierr)
    !nombre processeurs
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
    !quel est mon rang
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang, ierr)
    CALL SRAND(rang);

    WRITE(*, *) "MPI DerivedType from ", rang, "/", nprocs, "."
    P%x = RAND()
    P%y = RAND()
    WRITE(*,*) P%x

    CALL MPI_FINALIZE(ierr)

    CONTAINS
END PROGRAM MPI_Communication

