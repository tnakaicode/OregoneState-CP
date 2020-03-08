/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Code listing for MPIdeadlock.c
#include <stdio.h>
#include "mpi.h"
#define MAXLEN 100
main(int argc, char *argv[]) {
    int myrank, numprocs, fromrank, torank;
    char tosend[MAXLEN], received[MAXLEN];
		
    MPI_Status status;
    MPI_Init( &argc, &argv );
    MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
    MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
    if (myrank == 0) fromrank = numprocs - 1;
      else fromrank = myrank - 1;
    if ( myrank == numprocs - 1 ) torank = 0;
      else torank = myrank + 1;                     // Save string to send in tosend:
    sprintf( tosend, "Message sent from node %d to node %d\n", myrank, torank );
    MPI_Recv( received, MAXLEN, MPI_CHAR, fromrank, 0, MPI_COMM_WORLD, &status );
    MPI_Send( tosend,   MAXLEN, MPI_CHAR, torank,   0, MPI_COMM_WORLD );
    printf( "%s", received );                // Print string after successful receive
    MPI_Finalize();
    exit(0);
} 