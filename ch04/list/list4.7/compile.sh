gfortran -c ../list4.6/list4.6.f90
gfortran -c ../list4.5/list4.5.f90
gfortran -c ../list4.7/list4.7.f90
gfortran -o list4.7 list4.5.o list4.6.o list4.7.o
