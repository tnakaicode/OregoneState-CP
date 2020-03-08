/* 
************************************************************************
*  random.c:    A simple random number generator, not for serious work *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *  
*  comment: plot without connecting datapoints with lines              *
************************************************************************
*/
#include <stdio.h>

#define max 1000                      /* number of numbers generated */
#define seed 11                       /* seed for number generator */

main ()
{
   int i, old, newx, newy;
   
   FILE *output;          /* save data in badrand.dat */
   output = fopen("badrand.dat","w");
   
   old = seed;                        /* the seed */

   for (i=0; i<max; i++)              /* generating #max numbers */
   {
      newx = (57*old+1) % 256;        /* x-coordinate */
      newy = (57*newx+1) % 256;       /* y-coordinate */
      fprintf (output, "%i\t%i\n", newx, newy);
      old  = newy;
   }
   printf("data stored in badrand.dat.\n");
   fclose(output);
}
 
