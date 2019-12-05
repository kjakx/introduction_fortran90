#!/bin/bash

#PBS -l select=1
#PBS -q DP_002
#PBS -N list1-6

cd $PBS_O_WORKDIR
aprun ./list1-6 < input.txt
