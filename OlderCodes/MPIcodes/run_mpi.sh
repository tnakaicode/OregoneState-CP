#!/bin/bash -f
#
#---------------- SHORT COMMENT ----------------------------------------
# Template script for parallel MPI jobs to run on mphase Grid Engine cluster.
# Modify it for your case and submit to CODINE with
# command "qsub mpi_run.sh".

# You may want to modify the parameters for
# "-N" (job queue name), "-pe" (queue type and number of requested CPUs),
# "myjob" (your compiled executable).


# You can compile you code, for example myjob.c (*.f), with GNU mpicc or
#  mpif77 compilers as follows:
# "mpicc -o myjob myjob.c" or "mpif77 -o myjob myjob.f"

# You can monitor your jobs with command
# "qstat -u your_username" or "qstat -f" to see all queues.
# To remove your job, run "qdel job_id"
# To kill running job, use "qdel -f job_id"

# ------Attention: #$ is a special CODINE symbol, not a comment -----
#
#   The name, which will identify your job in the queue system
#$ -N MPI_job
#
#   Queue request, mpich. You can specify the number of requested CPUs,
#   for example, from 2 to 3
#$ -pe class_mpi 4-6
#
# ---------------------------
#$ -cwd
#$ -o  $HOME/output/$JOB_NAME-$JOB_ID
#$ -e  $HOME/output/error/$JOB_NAME-$JOB_ID.error
#$ -v  MPIR_HOME=/usr/local/cluster/mpich-1.2.6
# ---------------------------

echo "Got $NSLOTS slots."


#  Don't modify the line below if you don't know what it is
$MPIR_HOME/bin/mpirun -np $NSLOTS $1
