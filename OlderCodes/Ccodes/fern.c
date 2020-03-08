/* 
************************************************************************
*  fern.c: Create fractal, fern-like pattern                           *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down               *
*           Plot data without connecting datapoints with lines         *
************************************************************************
*/   
#include <stdio.h>
#include <stdlib.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */   
      
#define max 30000   /* number of iterations */
#define seed 68111    /* seed for number generator */
    
main() 
{    
   int i;
   double x, y, xn, yn, r;
   
   FILE *output;      /* save data in fern.dat */
   output=fopen("fern.dat","w");
   
   srand48(seed);     /* seed number generator */
   
   x     = 0.5;       /* starting point */
   y     = 0.0;
   
   for(i=1; i<=max; i++)                /* iterations */
   {
      r=drand48();
      if (r <= 0.02)      /* case 1 */
      { 
         xn = 0.5;
         yn = 0.27*y;
      }
      else if((r>0.02) && (r<=0.17))  /* case 2 */
      { 
         xn = -0.139*x + 0.263*y + 0.57;
         yn =  0.246*x + 0.224*y - 0.036;
      }
      else if ((r>0.17) && (r<=0.3))  /* case 3 */
      {
         xn = 0.17*x  - 0.215*y + 0.408;
         yn = 0.222*x + 0.176*y + 0.0893;
      }  
      else        /* case 4 */
      {
         xn = 0.781*x + 0.034*y + 0.1075;
         yn = -0.032*x + 0.739*y + 0.27;
      }  
      fprintf(output, "%f %f\n", x, y);   
      x=xn;
      y=yn;
   }
   printf("data stored in fern.dat\n");
   fclose(output);
}        
