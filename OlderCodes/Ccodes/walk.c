/* walk.c: Random walk simulation                                      *
* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                              *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down               *
*           Data is saved as sqrt(steps), distance                     */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */ 
                        
#define SQRT2 1.4142135623730950488E0
#define max 10000
#define seed 68111

main() {
   int i, j;
   double x, y;
   FILE *output;      /* save data in walk.dat */
   output = fopen("walk.dat","w");
   
   srand48(seed);                       /* seed the number generator */
   x=y=0;                            /* starting point  */
   for (i=1; i<=max; i++)  {
      x += (drand48()-0.5)*2 ;      /* dx and dy between -1 and 1 */
      y += (drand48()-0.5)*2 ;  
			fprintf(output, "%f\t%f\n", x, y);
    }
       printf("data stored in walk.dat.\n");
   fclose(output);
}
