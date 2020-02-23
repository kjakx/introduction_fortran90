gfortran -c ../list5.5/list5.5.f90
gfortran -c list5.6.f90
gfortran -o list5.6 list5.6.o list5.5.o
