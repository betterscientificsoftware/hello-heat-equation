# **How to Contribute**

We welcome conributions regarding bug or syntax fixes, new 1D Heat Equation functions in different languages, parallel paradigms, and library implementations, and codes in different versions of the Heat Equation including (but not limited to) different dimensions and the LaPlace Equation. Submitted codes should not be fully-working programs, but instead a markdown file with the language/library used as the title and the function(s) as a code block (See the [1D Heat Equation written in C](https://github.com/betterscientificsoftware/hello-heat-equation/blob/master/1DSerialHeat/C.md) as an example). All changes can be sumbitted via a pull request. Testing for proper syntax and runnability is left to the contributor. 

Below is a quick summary of the code layout and how to submit a pull request.

## Code Overview
The Heat Equation Showcase repository has the following structure:
  ```
  .
  ├── 1DLibImplementations
  │   ├── 1DExplicitHeat.md
  │   └── MFEM.md
  ├── 1DParallelHeat
  │   └── OpenMP.md
  ├── 1DSerialHeat
  │   ├── C.md
  │   ├── Fortran90.md
  │   ├── Julia.md
  │   ├── MatLab.md
  │   └── Python.md
  └── 3rdPartyHeatEquations.md
  ```
  
As shown, the folders in the root describe which Heat Equation the folder contains along with the implementation style (library, parallel, or serial implementation). Inside the library implementation folder contains markdown files titled the name of the library the code in the file contains. For the non-library implementation directories, the files inside are simply titled the name of the programming language/paradigm used. Currently, this repository only supports code relating to the 1D Heat Equation, but if different versions are contributed the file structure of the repository will change. For more information regarding the codes inside this repository, see the [README.md](https://github.com/betterscientificsoftware/hello-heat-equation/blob/master/README.md) in the root.

## Submitting a Pull Request
If you would like to submit new code into the repository, please follow the following steps:

## Contact Us!
For questions or concerns, submit an issue in this repository or send an email to Heather Switzer at hmswitzer@email.wm.edu.
