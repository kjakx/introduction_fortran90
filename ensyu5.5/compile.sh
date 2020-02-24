gfortran -c ifmod.f90
gfortran -c exsub.f90
gfortran -c main.f90
gfortran -o ensyu5.5 ifmod.o exsub.o main.o
