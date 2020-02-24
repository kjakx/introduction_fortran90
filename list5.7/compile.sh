gfortran -c list5.7.f90
gfortran -c exsub.f90
gfortran -c main.f90
gfortran -o list5.7 list5.7.o exsub.o main.o
