/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
/* TuneMPI.c: a matrix algebra program to be tuned for performace
 N X N Matrix speed tests using MPI  */

#include "mpi.h" 
#include <stdio.h> 
#include <time.h>
#include <math.h>

int main(int argc,char *argv[]) {
	 MPI_Status status;
	 time_t systime_i, systime_f;
   int N = 200, MAX = 15, h = 1, myrank, nmach, i, j, k, iter = 0;
	 long difftime = 0l;
   double ERR = 1.0e-6, dummy = 2., timempi[2], ham[N][N], coef[N], sigma[N];
   double ener[1], err[1], ovlp[1], mycoef[1], mysigma[1], myener[1], myerr[1];
   double myovlp[1], step = 0.0;
                                                                // MPI Initialization
   MPI_Init(&argc, &argv);
   MPI_Comm_rank( MPI_COMM_WORLD, &myrank );
   MPI_Comm_size( MPI_COMM_WORLD, &nmach );
   MPI_Barrier( MPI_COMM_WORLD );
   if ( myrank == 0 ) {
	   timempi[0] = MPI_Wtime();                                  // Store initial time
     systime_i = time(NULL);
   }
   printf("\n\t Processor %d checking in...\n", myrank);
   fflush(stdout);
   for ( i = 1; i < N; i++ ) {              // Set up Hamiltonian and starting vector
     for ( j = 1; j < N; j++ ) {
			 if ( abs(j-i) > 10 )  ham[j][i] = 0.0; 
         else  ham[j][i] = pow(0.3, abs(j-i));
      }
      ham[i][i] = i;
      coef[i] = 0.0;
    }
    coef[1] = 1.0  ;
    err[0] = 1.0;
    iter = 0 ;
    if ( myrank == 0 )  {                    //  Start iterating towards the solution
      printf( "\nIteration #\tEnergy\t\tERR\t\tTotal Time\n" );
      fflush(stdout);
    }
    while ( iter < MAX && err[0] > ERR ) {                        // Start while loop
      iter = iter + 1;
      mycoef[0]=0.0;
      ener[0] = 0.0; myener[0] = 0.0 ;
      ovlp[0] = 0.0; myovlp[0] = 0.0 ;
      err[0] = 0.0 ; myerr[0] = 0.0;
      for (i= 1; i < N; i++) {
        h = (int)(i)%(nmach-1)+1 ;
        if (myrank == h)  {
          myovlp[0] = myovlp[0]+coef[i]*coef[i];
          mysigma[0] = 0.0;
          for ( j= 1; j < N; j++ ) mysigma[0] =  mysigma[0] + coef[j]*ham[j][i];
          myener[0] =  myener[0]+coef[i]*mysigma[0] ;
          MPI_Send( &mysigma, 1, MPI_DOUBLE, 0, h, MPI_COMM_WORLD );
        }
      if ( myrank == 0 )  {
        MPI_Recv( &mysigma, 1, MPI_DOUBLE, h, h, MPI_COMM_WORLD, &status );
        sigma[i]=mysigma[0];
      }
    }                                                              // End of for(i...
    MPI_Allreduce( &myener, &ener, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD );
    MPI_Allreduce( &myovlp, &ovlp, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD );
    MPI_Bcast( &sigma, N-1, MPI_DOUBLE, 0, MPI_COMM_WORLD );
    ener[0] = ener[0]/(ovlp[0]);
    for (  i = 1; i< N; i++) { 
      h = (int)(i)%(nmach-1)+1 ;
      if (myrank == h) {
				mycoef[0] = coef[i]/sqrt(ovlp[0]);
        mysigma[0] = sigma[i]/sqrt(ovlp[0]);
        MPI_Send( &mycoef,  1, MPI_DOUBLE, 0, nmach+h+1,   MPI_COMM_WORLD );
        MPI_Send( &mysigma, 1, MPI_DOUBLE, 0, 2*nmach+h+1, MPI_COMM_WORLD );
      }
      if (myrank == 0) {
        MPI_Recv( &mycoef,  1, MPI_DOUBLE, h, nmach+h+1,   MPI_COMM_WORLD, &status );
        MPI_Recv( &mysigma, 1, MPI_DOUBLE, h, 2*nmach+h+1, MPI_COMM_WORLD, &status );
        coef[i]=mycoef[0];
        sigma[i]=mysigma[0];
      }
    }                                                             // End of for(i...
        MPI_Bcast( &sigma, N-1, MPI_DOUBLE, 0, MPI_COMM_WORLD );
        MPI_Bcast( &coef,  N-1, MPI_DOUBLE, 0, MPI_COMM_WORLD );
        for ( i = 2; i < N ; i++ )  {
          h = (int)(i)%(nmach-1)+1;
          if ( myrank == h )  {
            step = (sigma[i] - ener[0]*coef[i])/(ener[0]-ham[i][i]);
            mycoef[0] = coef[i] + step;
            myerr[0] =  myerr[0]+ pow(step,2);
            for ( k= 0; k <= N*N; k++ )                              // Slowdown loop
              { dummy = pow(dummy,dummy);          dummy = pow(dummy,1.0/dummy); }
            MPI_Send( &mycoef, 1, MPI_DOUBLE, 0, 3*nmach+h+1, MPI_COMM_WORLD );
          } // end of if(myrank..
          if (myrank == 0)  {
            MPI_Recv(&mycoef, 1, MPI_DOUBLE, h,3*nmach+h+1, MPI_COMM_WORLD, &status);
            coef[i]=mycoef[0];
          }
        }                                                          // End of for(i...
        MPI_Bcast(     &coef,  N-1,     MPI_DOUBLE, 0,       MPI_COMM_WORLD );
        MPI_Allreduce( &myerr, &err, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD );
        err[0] = sqrt(err[0]);
        if ( myrank==0 ) { printf("\t#%d\t%g\t%g\n", iter, ener[0], err[0]);
                    fflush(stdout); }
    }                                                                    // End while
    if (myrank == 0) {
      systime_f = time(NULL);                                  // Output elapsed time
      difftime = ((long) systime_f) - ((long) systime_i);
      printf( "\n\tTotal wall time = %d s\n", difftime );
      fflush(stdout);
      timempi[1] = MPI_Wtime();
      printf("\n\tMPItime= %g s\n", (timempi[1]-timempi[0]) );
      fflush(stdout);
    }
    MPI_Finalize();
}
