## 1D\_HEAT\_EXPLICIT Implementation

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
