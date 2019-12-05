#!/bin/bash

#PBS -l select=1
#PBS -q DP_002
#PBS -N list1-5

cd $PBS_O_WORKDIR
aprun ./list1-5 < input.txt
