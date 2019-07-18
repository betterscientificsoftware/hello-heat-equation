## Library Implementations
While a user may find it more comfortable to hand code their own implementation of the heat equation, it would be more beneficial to take advantage of pre-existing codes. Several libraries have already provided a code base that saves users time and allows them to access additional features the library may provide. Below are a couple of library implementations of the heat equation.

### MFEM
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

### FD1D\_HEAT\_EXPLICIT
[FD1D\_HEAT\_EXPLICIT](https://people.sc.fsu.edu/~jburkardt/cpp_src/fd1d_heat_explicit/fd1d_heat_explicit.html "FD1DHEATEXPLICIT") is a library that solves the 1D time-dependent Heat Equations. This library has been implemented in C, C++, Fortran77, Fortran90, MatLab, and Python. This version uses an explicit time stepping method but [FD1D\_HEAT\_IMPLICIT](https://people.sc.fsu.edu/~jburkardt/cpp_src/fd1d_heat_implicit/fd1d_heat_implicit.html "FD1DHEATIMPLICIT") uses an implicit method. The same authors also developed [HEAT\_PLATE](http://people.sc.fsu.edu/~jburkardt%20/c_src/heated_plate/heated_plate.html "HEATEDPLATE") which solves the 2D Heat equation in a rectangular area. The code for the 1D Heat Equation is shown below:
```c++
//This function takes one time step solving the 1D Heat Equation using an explicit method

double *fd1d_heat_explicit ( int x_num, double x[], double t, double dt,
  double cfl, double *rhs ( int x_num, double x[], double t ),
  void bc ( int x_num, double x[], double t, double h[] ), double h[] ){

  double *f;
  double *h_new;
  int j;

  f = rhs ( x_num, x, t );
  h_new = new double[x_num];
  h_new[0] = 0.0;

  for ( j = 1; j < x_num - 1; j++ ){
    h_new[j] = h[j] + dt * f[j]
      + cfl * (         h[j-1]
                - 2.0 * h[j]
                +       h[j+1] );
  }

  h_new[x_num-1] = 0.0;
  bc ( x_num, x, t + dt, h_new );
  delete [] f;
  return h_new;
}
```

__*For a full list of finite element software packages, see [this list from Wikipedia](https://en.wikipedia.org/wiki/List_of_finite_element_software_packages "Finite Element Software Packages").*__
