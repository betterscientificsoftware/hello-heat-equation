## "The Heat Equation is the 'Hello World' of Scientific Computing"

This repository is a showcase of 1D Heat Equation implementations.

### [1DSerialHeat](./1DSerialHeat/)
This file shows sequential implementations of a function that calculates a single iteration of the 1-dimensional heat equation using the forward in time, centered difference (FTCS) discretization written in several programming languages. These languages include C/C++, Fortran, Julia, Matlab, and Python. While some differences in syntax can be observed, the overall structure of the code stays consistent through the implementations. 

These examples were created by [@Heatherms27](https://github.com/Heatherms27)

### [1DParallelHeat](./1DParallelHeat)
This file currently only shows a parallel implementation of a function that calculates a single iteration of the 1-dimensional heat equation using the forward in time, centered difference (FTCS) discretization written using OpenMP with C as its base. This file will be expanded. 

This example was created by [@Heatherms27](https://github.com/Heatherms27)

### [1DLibImplementations/](./1DLibImplementations/)
While a user may find it more comfortable to hand code their own implementation of the heat equation, it would be more beneficial to take advantage of pre-existing codes. Several libraries have already provided a code base that saves users time and allows them to access additional features the library may provide. Below are a couple of library implementations of the heat equation.  
__*For a full list of finite element software packages, see [this list from Wikipedia](https://en.wikipedia.org/wiki/List_of_finite_element_software_packages "Finite Element Software Packages").*__

### [3rdPartyHeatEquations.md](./3rdPartyHeatEquations.md)
As a result of the Heat Equation being so expansive and taking on several different forms, many examples of heat equation implementations are readily availible through a quick Google search. This file contains a table that includes links to user implementations of the 1-dimensional, 2-dimensional, and LaPlace forms of the Heat Equation in C, C++, Fortran, Julia, Matlab, and Python. 
