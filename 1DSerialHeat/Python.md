## Serial Python Implementation
The input parameters are as follows:
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
