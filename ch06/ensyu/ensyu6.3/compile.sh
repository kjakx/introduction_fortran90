gfortran -c ifmod.f90
gfortran -c gauss_jordan_pv.f90
gfortran -c main.f90
gfortran -o ensyu6.3 ifmod.o gauss_jordan_pv.o main.o
