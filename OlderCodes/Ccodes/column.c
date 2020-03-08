/* 
************************************************************************
*  column.c: Correlated ballistic deposition  to form fractal          *
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
      
#define max 100000    /* number of iterations */
#define npoints 200         /* no. of open spaces */ 
#define seed 68111    /* seed for number generator */
    
main()
{    
   int i, hit[200], dist, r, x, y, oldx, oldy;
   double  pp, prob;
   
   FILE *output;
   output = fopen("column.dat","w");   
   
   srand48(seed);               /* seed the random number generator */
   
   for (i=0; i<npoints; i++) hit[i] = 0;  /* clear the array */
   
   oldx = 100;
   oldy = 0;
   
   for(i=1; i<=max; i++)
   {
      r = (int) (npoints*drand48()); 
      
      x = r-oldx; 
      y = hit[r]-oldy;
      dist = x*x + y*y;
      
      if(dist == 0)     /* probability of sticking */ 
      {         /* depends on distance to */
         prob = 1.0;      /* the last particle */
      }
      else prob = 9.0/(dist);               /* nu=-2.0, c=0.9 */
      
      pp = drand48();
      if(pp < prob)
      {
         if((r>0) && (r<(npoints-1)))
         {   
            if((hit[r] >= hit[r-1]) && (hit[r] >= hit[r+1]))
            {
               hit[r]++;
            }      
            else if(hit[r-1] > hit[r+1])
            {
               hit[r] = hit[r-1];
            }   
            else
            {
               hit[r] = hit[r+1];
            }
            oldx = r;
            oldy = hit[r];
            fprintf(output, "%d\t%d\n", r, hit[r]);  
         }
      }
   }
   printf("data stored in column.dat\n");
   fclose(output);
}
