gfortran -c ./ifmod.f90
gfortran -c ./main.f90
gfortran -c ./exsub.f90
gfortran -o ensyu4.1 main.o exsub.o ifmod.o
