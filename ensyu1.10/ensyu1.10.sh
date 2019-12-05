#!/bin/bash

#PBS -l select=1
#PBS -q DP_002
#PBS -N ensyu1.10

cd $PBS_O_WORKDIR
aprun ./ensyu1.10 < input.txt
