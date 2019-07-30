## Parallel FTCS Hand-Coded Implementations

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

### OpenMP (C)
```c
bool update_solution_ftcs(int n, double *uk1, const double *uk0,
double alpha, double dx, double dt, double bc0, double bc1){
    double r = alpha * dt / (dx * dx);

    // Sanity check for stability
    if (r > 0.5) return false;

    //FTCS update algorithm
    #pragma omp parallel num_threads(n-2) private(tid) shared(uk1, uk0, r){
        i = omp_get_thread_num();    // Thread numbers will range from 0 to n-3
        uk1[i+1] = r*uk0[i+2] + (1-2*r)*uk0[i+1] + r*uk0[i];
     }

    // Enforce boundary conditions
    uk1[0] = bc0;
    uk1[n-1] = bc1;

    return true;
}
```
