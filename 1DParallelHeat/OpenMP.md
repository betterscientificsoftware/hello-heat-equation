## Parallel OpenMP Implementation (with base C)

The input parameters are as follows:
```
n     = Number of temperature samples        // Integer
uk1   = New temperatures across x-axis       // Array of Doubles
uk0   = Old temperatures across x-axis       // Array of Doubles
alpha = Thermal Diffusity                    // Double
dx    = Spacing in space                     // Double
dt    = Spacing in time                      // Double
bc0   = Beginning boundary condition (x = 0) // Double
bc1   = End boundary condition (x = L_x)     // Double
```

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
or

When using a large number of n:
```c
bool update_solution_ftcs(int n, double *uk1, const double *uk0,
double alpha, double dx, double dt, double bc0, double bc1){
    double r = alpha * dt / (dx * dx);

    // Sanity check for stability
    if (r > 0.5) return false;

    //FTCS update algorithm
    #pragma omp parallel for shared(uk1, uk0, r)
    for (int i = 1; i < n - 1; i++) {
        uk1[i] = r*uk0[i+1] + (1-2*r)*uk0[i] + r*uk0[i - 1];
     }

    // Enforce boundary conditions
    uk1[0] = bc0;
    uk1[n-1] = bc1;

    return true;
}
```
This is beacuse the first solution is not going to scale beyond a certain number of threads as the it will fail to allocate stack space  - __on default settings__- for the all the to be created threads.
