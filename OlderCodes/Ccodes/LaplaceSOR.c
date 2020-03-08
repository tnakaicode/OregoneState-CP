/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  LaplaceSOR.c:Solve Laplace equation with finite difference method
//  Output data saved in 3D grid format of gnuplot  

#include<stdio.h>        
#include<math.h> 
#include<stdlib.h> 

main() {
	
  int  max= 40;                              // number of grid points
  double tol,omega,r;
  double p[40][40];
  int i, j, iter;
	
  FILE *output1;
  output1 = fopen("laplaceR.dat","w") ;  // save data in laplaceR.dat
  omega = 1.4;                                   // The SQR parameter
  for (i = 0; i<max; i++) for (j = 0; j<max; j++) p[i][j]= 0;// clear
  for (i = 0; i<max; i++) p[i][0]= +100.0;          // p[i][0]= 100 V
  tol= 1.0;                                               //tolerance
  iter= 0;                                              // iterations
  while ( (tol > 0.000001) && (iter <=  140) ){ 
    tol= 0.0;
    for (i = 1; i<(max-1); i++)   {                    // x-direction
      for (j = 1; j<(max-1); j++)  {                   // y-direction
        r= omega * ( p[i][j+1] + p[i][j-1] + p[i+1][j] +
                           p[i-1][j] - 4.0 * p[i][j] ) / 4.0;
        p[i][j] += r;
        if ( fabs(r) > tol ) tol= fabs(r);
      }
      iter++;
    }
 }
 for (i = 0; i<max ; i++) {           // write data gnuplot 3D format
   for (j = 0; j<max; j++) fprintf(output1,"%f\n",p[i][j]);
   fprintf(output1,"\n");	                  // empty line for gnuplot
 }
 printf("data stored in laplaceSOR.dat");
 }
