gfortran -c ifmod.f90
gfortran -c ensyu6.5.f90
gfortran -c main.f90
gfortran -o ensyu6.5 main.o ensyu6.5.o ifmod.o
