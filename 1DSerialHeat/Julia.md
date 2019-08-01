## Serial Julia Implementation
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
