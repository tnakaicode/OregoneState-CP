/* 
************************************************************************
*  sierpin.c: Creates Sierpinsky gasket fractal                        *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down.              *
*           Plot data without connecting datapoints with lines.        *
************************************************************************
*/   

#include <stdio.h>
#include <stdlib.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */   
      
#define max 30000   /* number of iterations */
#define seed 68111    /* seed for number generator */
#define a1 20.0     /* vertex 1 */
#define b1 20.0
#define a2 320.0    /* vertex 2 */
#define b2 20.0
#define a3 170.0    /* vertex 3 */
#define b3 280.0

main() 
{    
   int i;
   double x, y, r;
   
   FILE *output;      /* save data in sierpin.dat */
   output = fopen("sierpin.dat","w");
   
   x = 180.;        /* starting point */
   y = 150.;
   
   srand48(seed);     /* seed number generator */
   
   for(i=1 ; i<=max ; i++)              /*  draw the gasket  */
   {
      r  = drand48();
      if (r <= 0.3333)
      {
         x = 0.5*(x + a1);
         y = 0.5*(y + b1);
      }
      else if(r > 0.3333 && r <= 0.6666)
      {
         x = 0.5*(x + a2);
         y = 0.5*(y + b2);
      }   
      else 
      {
         x = 0.5*(x + a3);
         y = 0.5*(y + b3);
      }  
      fprintf(output, "%f %f\n", x, y);   
   }
   printf("data stored in sierpin.dat\n");
   fclose(output);
}
