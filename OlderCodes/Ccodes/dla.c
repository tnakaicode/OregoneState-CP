/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  dla.c: Diffusion-limited aggregation simulation (fractals)

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// if you don't have drand48 uncomment the following two lines 
//    #define drand48 1.0/RAND_MAX*rand 
//    #define srand48 srand                                   
#define max 40000                             // number of iterations
#define size 401                                // size of grid array
#define PI 3.1415926535897932385E0
#define seed 68111                       // seed for number generator

main()  {
	
   double angle, rad = 180.0;
   int i,j, x, y, dist,step, trav;
   int grid[size][size], hit;
   int gauss_ran();                         // Gaussian random number
	 
   FILE *output;                              // save data in dla.dat
   output = fopen("dla.dat","w");      
	                                                          // clear
   for (i=0; i<size; i++) for (j=0; j<size; j++) grid[i][j] = 0;  
   grid[200][200] = 1;                  // one particle at the center
	                                           // seed number generator
   srand48(seed);                                 
   for (i=0; i<max; i++)  {                  // choose starting point
     hit  = 0;
     angle = (2*PI*drand48());                        // random angle
     x    = (200+rad*cos(angle));                      // coordinates
     y    = (200+rad*sin(angle));
     dist = gauss_ran();               // random number gaussian dist
		                                    // move forwards or backwards
     if (dist<0) step = -1;                  
      else step = 1;
     trav=0;
     while((hit == 0) && (x<399) && (x>1) && (y<399) && (y>1) && 
		     (trav<abs(dist))) {
      if (grid[x+1][y]+grid[x-1][y]+grid[x][y+1]+grid[x][y-1]>= 1) {
				                                  // one neighbor is occupied
        hit = 1;                               
        grid[x][y] = 1;  
				                             // particle sticks, walk is over
      }
        else if (drand48() < 0.5) x+=step;       // move horizontally
          else                y+=step;             // move vertically
        trav++;
      }  
   }
   for (i=0; i<size; i++) for (j=0; j<size; j++)       //  print grid
		       if (grid[i][j] == 1) fprintf(output,"%d\t%d\n", i, j);
   printf("data stored in dla.dat\n");
   fclose(output);
}                                                         // end main
 
int gauss_ran()  {       
	                  // Box-Mueller method for Gaussian random numbers
  double fac, rr, r1, r2;
  static int old = 0;             // have to be static so information
  static int mem;                  // survives between function calls
  if (old == 0) {                            // no random number left
		                            // choose random point in unit circle
    do {
      r1 = 2.0*drand48()-1.0;                    
      r2 = 2.0*drand48()-1.0;                           
      rr = r1*r1+r2*r2;        
    }
		while ((rr>=1) || (rr==0));    
		  fac = sqrt(-2*log(rr)/rr);
      mem = 5000*r1*fac;                        // save for next call
      old = 1;                                            // set flag
     return((int)(5000*r2*fac)); 
   }
   else {                                     // return second number
     old = 0;                                           // unset flag
     return mem;                      // return number from last call
   }
}   
