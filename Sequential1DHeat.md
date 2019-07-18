## Sequential FTCS Hand-Coded Implementations

The input parameters for the hand-coded functions are as follows:
```
n     = Number of temperature samples        # Integer
uk1   = New temperatures across x-axis       # Array of Doubles
uk0   = Old temperatures across x-axis       # Array of Doubles
alpha = Thermal Diffusity                    # Double
dx    = Spacing in space                     # Double
dt    = Spacing in time                      # Double
bc0   = Beginning boundary condition (x = 0) # Double
bc1   = End boundary condition (x = L_x)     # Double
```

### C/C++
```c
bool update_solution_ftcs(int n, double *uk1, const double *uk0,
double alpha, double dx, double dt, double bc0, double bc1){
    double r = alpha * dt / (dx * dx);

    // Sanity check for stability
    if (r > 0.5) return false;

    //FTCS update algorithm
    for(int i = 1; i < n-1; ++i)
        uk1[i] = r*uk0[i+1] + (1-2*r)*uk0[i] + r*uk0[i-1];

    // Enforce boundary conditions
    uk1[0] = bc0;
    uk1[n-1] = bc1;

    return true;
}
```

### Fortran 90
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

### Julia
```julia
function update_solution_ftcs(n, uk1, uko, alpha, dx, dt, bc0, bc1)
    r = alpha * dt / (dx^2)


    if r > 0.5
        return false
    end

    # FTCS update algorithm
    for i = 2:n-1
        uk1[i] = r * uk0[i+1] + (1-2*r) * uk0[i] + r * uk0[i-1]
    end

    # Enforce boundary conditions. Julia has 1-based indexing.
    uk1[1] = bc0
    uk1[n] = bc1

    return uk1
end
```

### Matlab
```matlab
function uk1 = update_solution_ftcs(n, uk1, uk0, alpha, dx, dt, bc0, bc1)
    r = alpha * dt/dx^2;

    % Sanity check for stability
    if r > 0.5
        return
    end

    % FTCS update algorithm
    for i = 2:n-1
        uk1[i] = r*uk0[i+1] + (1-2*r)*uk0[i] + r*uk0[i-1];
    end

    % Enforce boundary conditions
    uk1[1] = bc0;
    uk1[n] = bc1;
end
```

### Python
```python
def update_solution_ftcs(n, uk1, uk0, alpha, dx, dt, bc0, bc1):
    r = alpha * dt / dx**2

    # Sanity check for stability
    if r > 0.5
        return false

    # FTCS update algorithm
    for i in range(1:n-1):
        uk1[i] = r*uk0[i+1] + (1-2*r)*uk0[i] + r*uk0[i-1]

    # Enforce boundary conditions
    uk1[0] = bc0
    uk1[n-1] = bc1

    return uk1
```

We can see with the simple 1 dimensional hand-coded implementations that no matter the programming language the codes look very similar with just some slight modifications to syntax.
