## Serial Fortran90 Implementation
The input parameters are as follows:
```
n     = Number of temperature samples        ! Integer
uk1   = New temperatures across x-axis       ! Array of Doubles
uk0   = Old temperatures across x-axis       ! Array of Doubles
alpha = Thermal Diffusity                    ! Double
dx    = Spacing in space                     ! Double
dt    = Spacing in time                      ! Double
bc0   = Beginning boundary condition (x = 0) ! Double
bc1   = End boundary condition (x = L_x)     ! Double
```

```fortran
SUBROUTINE update_solution_ftcs(n, uk1, uk0, alpha, dx, dt, bc0, bc1)
    INTEGER, INTENT(IN) :: n
    REAL, INTENT(IN) :: alpha, dx, dt, bc0, bc1
    REAL, DIMENSION(n), INTENT(IN) :: uk0
    REAL, DIMENSION(n), INTENT(OUT) :: uk1

    REAL :: r
    r = alpha * dt/(dx**2)

    ! Sanity check for stability
    IF (r > 0.5) THEN
        RETURN
    END IF

    ! FTCS update algorithm
    DO i = 2, n-1
        uk1(i) = r*uk0(i+1) + (1-2*r)*uk0(i) + r*uk0(i-1)
    END DO

    ! Enforce boundary conditions
    uk1(1) = bc0
    uk1(n) = bc1

END SUBROUTINE update_solution_ftcs
```
