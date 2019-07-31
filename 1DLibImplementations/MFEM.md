## Condensed MFEM Implementation
[MFEM](https://mfem.org/ "MFEM") is an opensource C++ library for finite element methods. Their serial implementation of the Heat Equation can be found [here](https://mfem.github.io/doxygen/html/ex16_8cpp_source.html "ex16.cpp"), though they also produce a parallelized version of this code. A (very) condensed version of the code can be shown below:

```c++
// 1. Read mesh from a given mesh file
Mesh *mesh = new Mesh(mesh_file, 1, 1);
int dim = mesh->Dimension();

// 2. Define ODE solver type
switch(ode_solver_type){
// Cases
}

// 3. Refine mesh to increase resolution
for (int lev = 0; lev < ref_levels; lev++){
    mesh->UniformRefinement();
}

// 4. Define vector element space, initial conditions for u, and conductor operator
H1_FECollection fe_coll(order, dim);
FiniteElementSpace fespace(mesh, &fe_coll);
FunctionCoefficient u_0(InitialTemperature);
u_gf.ProjectCoefficient(u_0);
Vector u;
u_gf.GetTrueDofs(u);
ConductionOperator oper(fespace, alpha, kappa, u);

// 5. Perform time-integration
ode_solver->Init(oper);
double t = 0.0;
bool last_step = false;
for (int ti = 1; !last_step; ti++){
// Time integration at each step
}

// 6. Save the final solution that can be viewed later
ofstream osol("ex16-final.gf");
osol.precision(precision);
u_gf.Save(osol);

```
