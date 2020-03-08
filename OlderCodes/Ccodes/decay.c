/* 
************************************************************************
*  decay.c: Spontaneous radioactive decay simulation                   *
*                                                                      *
*  taken from: "Projects in Computational Physics" by Landau and Paez  * 
*        copyrighted by John Wiley and Sons, New York                  *      
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
************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */   
      
#define lambda 0.01                      /* the decay constant */
#define max 1000                         /* number of atoms at t=0 */
#define time_max 500                     /* time range */
#define seed 68111                       /* seed for number generator */

main()
{
   int atom, time, number, nloop;
   double decay;
   
   FILE *output;       /* save data in decay.dat */
   output = fopen("decay.dat","w");
   
   number = nloop = max;                 /* initial value */
   srand48(seed);                        /* seed number generator */
   
   for(time=0; time<=time_max; time++)         /* time loop */ 
   {
      for (atom=1; atom<=number; atom++)       /* atom loop */
      {
         decay = drand48();
         if(decay < lambda) nloop--;           /* an atom decays */
      }     
      number = nloop;
      fprintf(output, "%d\t%f\n", time, (double)number/max);
   }
   printf("data stored in decay.dat\n");
   fclose(output);
}
 
