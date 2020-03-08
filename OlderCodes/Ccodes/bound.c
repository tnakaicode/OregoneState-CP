/*
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Objected Oriented Programming of accelerated motion in 2D         c
 * From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                     c
c  UNIX (DEC OSF, IBM AIX): cpp accm2d.cpp                            c
c                                                                     c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
                                                                   *
*  comment: LAPACK has to be installed on your system and the file     *
*     gauss.c has to be in the same directory                          *
*     The energy eigenvalue is printed to standard output and          *
*     the corresponding eigenvector to the file bound.dat              *
************************************************************************
*/
#include <stdio.h>
#include <math.h>
#include "gauss.c" 

#define min 0.0                         /* integration limits  */
#define max 200.0 
#define size 64                         /* grid points */
#define lambda -1.0                     /* parameters for potential  */
#define u 0.5
#define b 2.0 
#define PI 3.1415926535897932385E0

main()
{
   int i, j , c1, c2, c5, ok;
   char c3, c4;
   double A[size][size], AT[size*size];         /* hamiltonian */
   double V[size][size];      /* potential */
   double WR[size], WI[size];               /* eigenvalues */ 
   double VR[size][size], VL[1][1];       /* eigenvectors */  
   double WORK[5*size];                         /* work space */  
   double k[size], w[size];               /* points, weights */
              /* for integration */
   FILE *out;       /* save data in bound.dat */
   out = fopen("bound.dat","w");
            
   gauss(size, 0, min, max, k, w);      /* call gauss integration */

  
   for(i=0; i<size; i++)          /* set up hamiltonian matrix */
   {        
      for(j=0; j<size; j++)
      {
         VR[i][j] = (lambda*b*b/(2*u))*
          (sin(k[i]*b)/(k[i]*b))*(sin(k[j]*b)/(k[j]*b));
         if(i == j)
         {
            A[i][j] = k[i]*k[i]/(2*u) + (2/PI)*VR[i][j]*k[j]*k[j]*w[j];
         }
         else A[i][j]=(2/PI)*VR[i][j]*k[j]*k[j]*w[j];
      }
   }
   
   for(i=0; i<size; i++)          /* transform matrix */
   {          /* so we can pass it to */
      for(j=0; j<size; j++)   /* a FORTRAN routine */
      {
         AT[j+size*i] = A[j][i];
      }
   }
   c1 = size;                   /* we have to do this so we can */
   c3 = 'N';      /* pass pointers to the lapack */ 
   c4 = 'V';      /* routine */
   c5 = 5*size; 
   c2=1;      /* for unreferenced arrays */
   /* call for AIX  */
   dgeev(&c3,&c4,&c1,AT,&c1,WR,WI,VL,&c2,VR,&c1,WORK,&c5,&ok); 
   
   /* call for DEC  */     
/* dgeev_(&c3,&c4,&c1,AT,&c1,WR,WI,VL,&c2,VR,&c1,WORK,&c5,&ok); */
   
   if(ok == 0)        /* look for bound state */  
   {
      for (j=0; j<size; j++)
      { 
         if (WR[j]<0)
         {
            printf("The eigenvalue of the bound state is\n");
            printf("\tlambda= %f\n", WR[j]);
            for (i=0; i<size; i++)
            {
               fprintf(out, "%d\t%e\n", i, VR[j][i]);
            }
            break; 
         }
      }
   }
   printf("eigenvector saved in bound.dat\n");
   fclose(out);
}  

