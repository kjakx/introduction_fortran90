#!/bin/bash

#PBS -l select=1
#PBS -q DP_002
#PBS -N ensyu1.8

cd $PBS_O_WORKDIR
aprun ./ensyu1.8 < input.txt
