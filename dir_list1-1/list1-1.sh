#!/bin/bash

#PBS -l select=1
#PBS -q DP_002
#PBS -N list1-1

cd $PBS_O_WORKDIR
aprun ./a.out
