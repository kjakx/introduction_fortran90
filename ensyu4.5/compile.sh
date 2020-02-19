gfortran -c ifmod.f90
gfortran -c ../ensyu4.4/ensyu4.4.f90
gfortran -c ensyu4.5.f90
gfortran -c main.f90
gfortran -o ensyu4.5 main.o ensyu4.5.o ensyu4.4.o ifmod.o
