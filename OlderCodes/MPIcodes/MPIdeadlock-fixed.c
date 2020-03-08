/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*/  
// deadlock-fixed.c: MPI deadlock.c without deadlock by Phil Carter
#include <stdio.h>
#include "mpi.h"
#define MAXLEN 100

main(int argc, char *argv[])  {
  int myrank, numprocs, torank, i;
  char tosend[MAXLEN], received[MAXLEN];
  MPI_Status status;
  MPI_Init( &argc, &argv );
  MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs );
  if ( myrank == numprocs - 1 ) torank = 0;
    else torank = myrank + 1;                       // Save string to send in tosend:
  sprintf( tosend, "Message sent from node %d to node %d\n", myrank, torank );
  for ( i = 0; i < numprocs; i++ )  {
    if ( myrank == i ) MPI_Send(tosend, MAXLEN, MPI_CHAR, torank, i, MPI_COMM_WORLD);
        else if ( myrank == i+1 || (i == numprocs - 1 && myrank == 0) )
            MPI_Recv( received, MAXLEN, MPI_CHAR, i, i, MPI_COMM_WORLD, &status );
    }
    printf("%s", received);                  // Print string after successful receive
    MPI_Finalize();
    exit(0);
}
