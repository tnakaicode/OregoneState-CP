/* 
************************************************************************
*  tree.c: Creates a fractal pattern that looks like a tree            *
*                      *
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
   double x,y,r,xn,yn;
   
   FILE *output;    /* save data in tree.dat */
   output=fopen("tree.dat","w");
   
   srand48(seed);   /* seed number generator */
   
   x     = 0.5;     /* initial position */
   y     = 0.0;
  
   for(i=1 ; i<=max; i++)
   {
      r = drand48();
      if (r<=0.1)
      { 
         xn = 0.05*x;
         yn = 0.6*y;
      }
      else if((r>0.1) && (r<0.2))
      { 
         xn  = 0.05*x;
         yn  = -0.5*y+1.0;
      }
      else if ((r>0.2) && (r<0.4))
      {
         xn  = 0.46*x-0.32*y;
         yn  = 0.39*x+0.38*y+0.6;
      }  
      else if((r>0.4) && (r<0.6))
      {
         xn  = 0.47*x-0.15*y;
         yn  = 0.17*x+0.42*y+1.1;
      }
      else if ((r>0.6) && (r<0.8))
      {
         xn  = 0.43*x+0.28*y;
         yn  = -0.25*x+0.45*y+1.0;
      }  
      else 
      {
         xn  = 0.42*x+0.26*y;
         yn  = -0.35*x+0.31*y+0.7;
      }  
      fprintf(output, "%f %f\n", xn, yn);   
      x = xn;
      y = yn;
   }
   printf("data stored in tree.dat\n");
   fclose(output);
}        
      
