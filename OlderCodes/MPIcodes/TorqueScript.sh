#!/bin/bash
#
# All lines starting with "#PBS" are PBS commands
#
# Request 2 nodes with 2 processor per node (ppn) (= 4 processors)
# ppn can either be 1 or 2
#
#PBS -l nodes=2:ppn=2
#
# Set wall clock max time to 0 hours, 15 minutes and 0 seconds
#PBS -l walltime=00:15:00
#
# cd to working directory
cd $PBS_O_WORKDIR
# name of executable
myprog=MPIpi
#
# Number of processors is $NP
NP=4
#
# Run MYPROG with appropriate mpirun script
mpirun -r ssh -n $NP $myprog
#
# make sure to exit the script, else job won't finish properly
exit 0 