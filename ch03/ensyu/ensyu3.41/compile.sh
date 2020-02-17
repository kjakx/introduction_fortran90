# correct order
# gfortran -o ensyu3.41 ../list3.34/list3.34.f90 ../list3.35/list3.35.f90 ../list3.36/list3.36.f90
# wrong order
# gfortran -o ensyu3.41 ../list3.35/list3.35.f90 ../list3.34/list3.34.f90 ../list3.36/list3.36.f90
gfortran -c ../list3.34/list3.34.f90
gfortran -c ../list3.35/list3.35.f90 
gfortran -c ../list3.36/list3.36.f90
gfortran -o ensyu3.41 list3.34.o list3.35.o list3.36.o
