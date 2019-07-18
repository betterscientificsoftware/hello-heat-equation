## "The Heat Equation is the 'Hello World' of Scientific Computing"

The files listed in this repository are Sequential1DHeat.md and LibraryImplementations.md

## Sequential1DHeat.md
This file shows sequential implementations of a function that calculates a single iteration of the 1 dimensional heat equation using the forward in time, centered difference (FTCS) algorithm written in several programming languages. These languages include C/C++, Fortran, Julia, Matlab, and Python. While some differences in syntax can be observed, the overall structure of the code stays consistent through the implementations. 

## LibraryImplmentations.md
While users can hand code their own implementation of the heat equation, it is usually not recommended for more complicated data sets. This file shows a couple examples of how heat is coded in the numerical libraries MFEM and FD1D\_HEAT\_EXPLICIT. Using numerical libraries allows the user to save time by not reinventing the wheel and access different features the library may provide.
