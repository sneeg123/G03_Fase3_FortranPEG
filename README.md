# FortranPEG
Parser generator for Fortran using PEG and a recursive descent parser.

[https://ecys-fiusac.github.io/fortranpeg/fases/fase2/](https://ecys-fiusac.github.io/fortranpeg/fases/fase2/)

``` shell
gfortran -c parser.f90
gfortran -c test.f90
gfortran -o test test.o parser.o
```