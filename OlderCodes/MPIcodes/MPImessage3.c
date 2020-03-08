/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*/  
//  MPImessage3.c: guests send rank to the host, who prints them
#include "mpi.h"
#include <stdio.h>

int main(int argc,char *argv[])  {   
  int rank, size, msg_size = 6, tag = 10, host = 0, n[1], r[1], i;      // 1-D Arrays
 
	MPI_Status status;
  MPI_Init( &argc, &argv );                                         // Initialize MPI
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );                           // Get CPU's rank
  MPI_Comm_size( MPI_COMM_WORLD, &size );                       // Get number of CPUs
  if ( rank != host ) { 
    n[0] = rank;
    printf("node %d about to send message\n", rank);         
    MPI_Send( &n, 1, MPI_INTEGER, host, tag, MPI_COMM_WORLD );   
  }
  else  { 
      for ( i = 1; i < size; i++ )  { 
      MPI_Recv( &r, 1, MPI_INTEGER, MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, &status );
      printf("Message recieved:  %d\n", r[0]);  }  
  }
  MPI_Finalize();                                          // Finalize MPI
  return 0;
}
