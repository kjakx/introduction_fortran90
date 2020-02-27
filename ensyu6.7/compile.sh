gfortran -c ifmod.f90
gfortran -c ensyu6.7.f90
gfortran -c main.f90
gfortran -o ensyu6.7 main.o ensyu6.7.o ifmod.o
