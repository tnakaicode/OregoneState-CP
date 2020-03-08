 /* 
************************************************************************
*  qmc.c:  Feynman path integral (quantum Monte Carlo) for             *
*   ground state wave function                                         *
*                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down.              *
*     This might take a couple of minutes.                             *
************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */
      
#define max 250000      /* number of trials */
#define seed 68111      /* seed for number generator */

main()
{
   double change, newE, oldE, path [101];
   int i, j, element, prop [101];
   
   double energy( double array[]);     /* finds energy of path */
   
   FILE *output;                           /* save data in qmc.dat */
   output = fopen("qmc.dat","w");
   
   srand48(seed);        /* seed number generator */
   
   for (j=0; j<=100; j++) path[j]=0.0;     /* initial path */
   for (j=0; j<=100; j++) prop[j]=0;       /* initial probability */
   
   oldE = energy(path);                    /* find energy of path */

   for (i=0; i<max; i++)
   {
      element = drand48()*101;             /* pick one random element */
      change  = (int)((drand48()-0.5)*20)/10.0;   /* change -0.9..0.9 */
      
      path[element]+=change;       /* change path */
      
      newE = energy(path);                 /* find the new energy  */
      
      if ((newE>oldE) && (exp(-newE+oldE) <= drand48()))
      {
         path[element] -= change;    /* reject */
      }
      
      for (j=0; j<=100; j++)              /* add up probabilities  */
      {
         element = path[j]*10+50;
         prop[element]++; 
      }
      oldE = newE;
   }

   for (i=0; i<=100; i++)
   {
      fprintf(output, "%d\t%f\n", i-50, (double) prop[i]/max);
   }
   printf("data stored in qmc.dat\n");
   fclose(output);
}
/*------------------------end of main program-------------------------*/

/* function returns energy of the path configuration */
double energy (double array[])
{
   int i;
   double sum=0.;
   
   for (i=0; i<100; i++)
   {
      sum += pow(array[i+1]-array[i], 2.0) + array[i]*array[i];
   }   
   return (sum);
}
