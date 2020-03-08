/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  eqheat.c: Solution of heat equation using with finite differences

#include <stdlib.h> 
#include <stdio.h>
#define size 101                                         // grid size
#define max 30000                                       // iterations
#define thc 0.12                              // thermal conductivity
#define sph 0.113                                    // specific heat
#define rho 7.8                                            // density

main()  { 
	
   int i,j;
   double cons, u[101][2];     
	 
   FILE *output;                           // save data in eqheat.dat 
   output= fopen("eqheat.dat","w");
	                                           // t = 0 points are 100C
   for (i = 0; i<size; i++) u[i][0] = 100.;      
	                                            // except the endpoints
   for (j = 0; j<2; j++) {                          
     u[0][j]     = 0.;
     u[size-1][j]= 0.;
   } 
   cons = thc/(sph*rho);                        // material constants
	                                         // loop over max timesteps
   for (i = 1; i<= max; i++) {                  
		                                               // loop over space
     for (j = 1; j<(size-1); j++)                       
      {u[j][1]= u[j][0]+cons*(u[j+1][0]+u[j-1][0]-2.0*u[j][0]); }
		                                         // save every 1000 steps
      if ((i%1000 == 0) || (i == 1))  {   
        for (j = 0 ; j<size; j++) fprintf(output, "%f\n", u[j][1]);
        fprintf(output, "\n");              // empty line for gnuplot
      }
      for (j = 0; j<size; j++) u[j][0] = u[j][1];       // new to old
   }
   printf("data stored in eqheat.dat\n");
   fclose(output);
}      
   
