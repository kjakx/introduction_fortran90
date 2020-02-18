gfortran -c ifmod.f90
gfortran -c exsub.f90
gfortran -c main.f90
gfortran -o ensyu4.3 ifmod.f90 exsub.f90 main.f90
