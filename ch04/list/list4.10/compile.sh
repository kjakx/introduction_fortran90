gfortran -c ../list4.8/list4.8.f90
gfortran -c ../list4.9/list4.9.f90
gfortran -c list4.10.f90
gfortran -o list4.10 list4.10.o list4.9.o list4.8.o
