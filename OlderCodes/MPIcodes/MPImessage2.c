/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*/  
//  MPImessage2.c:  source node sends message to dest
#include "mpi.h"
#include <stdio.h>

int main(int argc,char *argv[])  {   
  int rank, msg_size = 6, tag = 10, source = 0, dest = 1;
	
  MPI_Status status;
  MPI_Init( &argc, &argv );                                         // Initialize MPI
  MPI_Comm_rank( MPI_COMM_WORLD, &rank );                           // Get CPU's rank
  if ( rank == source ) { 
    char *msg = "Hello";
    printf("Host about to send message: %s\n",msg);  // Send, may block till recieved
    MPI_Send( msg, msg_size, MPI_CHAR, dest, tag, MPI_COMM_WORLD ); 
  }
    else if ( rank == dest ) { 
      char buffer[msg_size+1];                                            // Receive
      MPI_Recv( &buffer, msg_size, MPI_CHAR, source, tag, MPI_COMM_WORLD, &status );
      printf("Message recieved by %d: %s\n", rank, buffer); 
    }
      printf("NODE %d done.\n", rank);                            //  All nodes print
      MPI_Finalize();                                                 // Finalize MPI
      return 0;
}
