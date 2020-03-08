/* 
************************************************************************
*  int_10d.c: Ten dimensional Monte-Carlo integration                  *
*                      *
c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c                                                       *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down               *
************************************************************************
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */     

#define max 65536     /* number of trials */

main()
{
   int i,j;
   double n=1.0,x,y=0;

   FILE *output;      /* save data in int_10d.dat */
   output= fopen("int_10d.dat", "w"); 

   for (i=1; i<=max; i++)
   {
      x=0;          /* reset x */
   
      for (j=1; j<=10; j++) x+= drand48();  /* sum of 10 x values */
   
      y+=x*x;       /* square and sum up */
   
      if (i%(int)(pow(2.0,n))==0) /* save after 2, 4, 8, 16 ... */ 
      {
         n++;
         fprintf( output, "%i\t\t%f\n", i, y/i);
      }     
   }
printf("data saved in int_10d.dat\n");       
fclose(output);
}
