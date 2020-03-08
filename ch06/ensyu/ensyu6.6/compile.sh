gfortran -c ifmod.f90
gfortran -c ensyu6.6.f90
gfortran -c main.f90
gfortran -o ensyu6.6 main.o ensyu6.6.o ifmod.o
