/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*/  
//  MPIhello.c         has each processor prints hello to screen
#include "mpi.h" 
#include <stdio.h>

int main( int argc, char *argv[] ) { 
  int myrank;
  MPI_Init( &argc, &argv );                                         // Initialize MPI
  MPI_Comm_rank( MPI_COMM_WORLD, &myrank );                         // Get CPU's rank
  printf( "Hello World from processor %d\n", myrank );
  MPI_Finalize( );                                                    // Finalize MPI
  return 0;
}
