/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*/  
// Code listing for MPIstring.c
#include <stdio.h>
#include <math.h>
#include "mpi.h"
#define maxt 10000                                    // Number of time steps to take
#define L 10000                                  // Number of divisions of the string
#define rho 0.01                                         // Density per length (kg/m)
#define ten 40.0                                                       // Tension (N)
#define deltat 1.0e-4                                                  // Delta t (s)
#define deltax .01                                                     // Delta x (m)
#define skip 50                       // Number of time steps to skip before printing
/* Need sqrt(ten/rho) <= deltax/deltat for a stable solution
                 Decrease deltat for more accuracy, c' = deltax/deltat  */

main(int argc, char *argv[]) {
  const double scale = pow( deltat/deltax, 2) * ten/rho; 
  int i, j, k, myrank, numprocs, start, stop, avgwidth, maxwidth, len;
  double left, right, startwtime, init\_string(int index);
  FILE *out;
	
  MPI_Init( &argc, &argv );
  MPI_Comm_rank( MPI_COMM_WORLD, &myrank );                            // Get my rank
  MPI_Comm_size( MPI_COMM_WORLD, &numprocs );                 // Number of processors
  MPI_Status status;
    if (myrank == 0)  {
      startwtime = MPI_Wtime();
      out = fopen("eqstringmpi.dat","w");     
    }
   // assign string to each node 1st and last points (0 and L-1) -must =0, else error
               // Thus L-2 segments for numprocs processors
    avgwidth = (L-2)/numprocs;
    start = avgwidth*myrank+1;
    if (myrank < numprocs - 1)  stop = avgwidth*(myrank+1);
    else stop = L-2;
    if (myrank == 0)   maxwidth = L-2 - avgwidth*(numprocs-1);
    else maxwidth = 0;
    double results[maxwidth];                               // Holds print for master
    len = stop - start;                                    // Length of the array - 1
    double x[3][len+1];         
    for (i = start; i <= stop; i++)   x[0][i-start] = init_string(i);
    x[1][0] = x[0][0]+0.5*scale*(x[0][1]+init_string(start-1)-2.*x[0][0]); //1st step
    x[1][len] = x[0][len] +0.5*scale*(init_string(stop+1)+x[0][len-1]-2.0*x[0][len]); 
    for (i = 1; i < len; i++)
        { x[1][i] = x[0][i] + 0.5*scale*(x[0][i+1] + x[0][i-1] - 2.0*x[0][i]); }
    for(k=1; k<maxt; k++) {                                      //  Later time steps
      if (myrank == 0) { MPI_Send( &x[1][len], 1, MPI_DOUBLE, 1, 1, MPI_COMM_WORLD);
                                               left = 0.0; } // Send to R, get from L
      else if (myrank < numprocs - 1) MPI_Sendrecv(&x[1][len],1,
MPI_DOUBLE, myrank+1, 1, &left, 1, MPI_DOUBLE, myrank-1, 1, MPI_COMM_WORLD, &status);
      else MPI_Recv( &left, 1, MPI_DOUBLE, myrank-1, 1, MPI_COMM_WORLD, &status );
      if (myrank == numprocs - 1) {                         // Send to L & get from R
        MPI_Send( &x[1][0], 1, MPI_DOUBLE, myrank-1, 2, MPI_COMM_WORLD );
        right = 0.0;            
      }
      else if (myrank > 0) MPI_Sendrecv( &x[1][0], 1, MPI_DOUBLE, myrank-1, 2,
                   &right, 1, MPI_DOUBLE, myrank+1, 2, MPI_COMM_WORLD, &status);
      else MPI_Recv( &right, 1, MPI_DOUBLE, 1, 2, MPI_COMM_WORLD, &status );
      x[2][0] = 2.*x[1][0] - x[0][0] + scale * (x[1][1] + left - 2.*x[1][0]);
      for (i = 1; i < len; i++)  
				{ x[2][i] = 2.*x[1][i] - x[0][i] + scale *(x[1][i+1]+x[1][i-1]-2.*x[1][i]); }
      x[2][len] = 2.*x[1][len] - x[0][len] + scale*(right+x[1][len-1] -2.*x[1][len]);
      for (i = 0; i <= len; i++)  { x[0][i] = x[1][i];  x[1][i] = x[2][i]; }
      if( k%skip == 0) {                        // Print using gnuplot 3D grid format
        if (myrank != 0) MPI_Send(&x[2][0], len+1, MPI_DOUBLE, 0, 3, MPI_COMM_WORLD);
        else {
          fprintf(out,"%f\n",0.0);                         // Left edge of (always 0)
          for ( i=0; i < avgwidth; i++ ) fprintf(out,"%f\n",x[2][i]);
          for ( i=1; i < numprocs-1; i++ ) { 
            MPI_Recv( results, avgwidth, MPI_DOUBLE, i, 3, MPI_COMM_WORLD, &status );
            for (j = 0; j < avgwidth; j++) fprintf(out, "%f\n",results[j]);  
          }
   MPI_Recv( results, maxwidth, MPI_DOUBLE, numprocs-1, 3, MPI_COMM_WORLD, &status );
          for ( j=0; j < maxwidth; j++ ) fprintf(out,"%f\n",results[j]);
          fprintf(out,"%f\n",0.0);                                          // R edge
          fprintf(out,"\n");                                // Empty line for gnuplot
        }  
    }  
  }
  if (myrank == 0)
    printf("Data stored in eqstringmpi.dat\nComputation time: %f s\n", 
	          MPI_Wtime()-startwtime);
    MPI_Finalize();
    exit(0);
}

  double init_string(int index) {
    if (index < (L-1)*4/5) return 1.0*index/((L-1)*4/5);
    return 1.0*(L-1-index)/((L-1)-(L-1)*4/5);
               // Half of a sine wave
  } 