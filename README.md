# "The Heat Equation is the 'Hello World' of Scientific Computing"

This repository is a showcase of 1D Heat Equation implementations.

## [Sequential1DHeat.md](./Sequential1DHeat.md)
This file shows sequential implementations of a function that calculates a single iteration of the 1-dimensional heat equation using the forward in time, centered difference (FTCS) discretization written in several programming languages. These languages include C/C++, Fortran, Julia, Matlab, and Python. While some differences in syntax can be observed, the overall structure of the code stays consistent through the implementations. 

These examples were created by [@Heatherms27](https://github.com/Heatherms27)

## [Parallel1DHeat.md](./Parallel1DHeat.md)
This file currently only shows a parallel implementation of a function that calculates a single iteration of the 1-dimensional heat equation using the forward in time, centered difference (FTCS) discretization written using OpenMP with C as its base. This file will be expanded. 

This example was created by [@Heatherms27](https://github.com/Heatherms27)

## [3rdPartyHeatEquations.md](./3rdPartyHeatEquations.md)
As a result of the Heat Equation being so expansive and taking on several different forms, many examples of heat equation implementations are readily availible through a quick Google search. This file contains a table that includes links to user implementations of the 1-dimensional, 2-dimensional, and LaPlace forms of the Heat Equation in C, C++, Fortran, Julia, Matlab, and Python. 

## [LibraryImplmentations.md](./LibraryImplementations.md)
While users can hand code their own implementation of the heat equation, it is usually not recommended for more complicated data sets. This file shows a couple examples of how heat is coded in the numerical libraries MFEM and FD1D\_HEAT\_EXPLICIT. Using numerical libraries allows the user to save time by not reinventing the wheel and access different features the library may provide.


