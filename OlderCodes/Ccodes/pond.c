/* 
************************************************************************
*  pond.c: *Monte-Carlo integration to determine pi (stone throwing)   *  
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                        *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down               *
************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */                   
      
#define max 1000                     /* number of stones to be thrown */
#define seed 68111                   /* seed for number generator */

main()
{
   int i, pi=0;
   double x, y, area;
   
   FILE *output;         /* save data in pond.dat */
   output = fopen("pond.dat","w");
   srand48(seed);                    /* seed the number generator */
   

   for (i=1; i<=max; i++)
   {
      x = drand48()*2-1;             /* creates floats between */
      y = drand48()*2-1;             /* 1 and -1 */
      if ((x*x + y*y)<1) pi++;       /* stone hit the pond */
      area=4*(double)pi/i;           /* calculate area */

      fprintf(output, "%i\t%f\n", i, area);
   }
   printf("data stored in pond.dat\n");
   fclose(output);
}
 
