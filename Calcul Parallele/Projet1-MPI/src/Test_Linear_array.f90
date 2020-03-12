! Created by  on 12/03/2020.

function Test_Linear_array()
    INTEGER ::ll, nn

    INTEGER :: N, Nik, m
    Ny = 3
    Nx = 3
    Nk = 2

    N = 4

    DO ll = 1, Ny
        DO m = 1, Nx
            DO nn = 1, Nk
                N = (ll - 1) * Nx * Nk + (m - 1) * Nk + nn
                CALL LINEARTO2D(N, Nk, Nx * Ny, k, Nik)
                CALL LINEARTO2D(Nik, Nx, Ny, i, j)
                !                j = (N-1) / (Nx*Nk) + 1
                !                i = MOD(N-1,Nx*Nk) / Nk + 1
                !                k = MOD(MOD(N-1,Nx*Nk),Nk)+1
                PRINT*, ll, m, nn
                PRINT*, j, i, k
                Print*, ''
            END DO
        END DO
    END DO
    STOP
end function Test_Linear_array