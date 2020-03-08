/*
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Objected Oriented Programming of accelerated motion in 2D         c
 * From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                     c
c  UNIX (DEC OSF, IBM AIX): cpp accm2d.cpp                            c
c                                                                     c                                                               *
*  comment: If your compiler complains about drand48, srand48          *
*           uncomment the define statements further down.              *
************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* if you don't have drand48 uncomment the following two lines */
/*    #define drand48 1.0/RAND_MAX*rand 
      #define srand48 srand                */  
      
main()
{
   int i, seed;
   double x;
   
   printf("enter seed\n");                    /* user plants seed */
   scanf("%i", &seed);
   
   srand48(seed);                             /* seed drand 48 */
   
   for (i=1; i<=10; i++) 
   {
      x = drand48();              /* random number between 0 and 1 */
      printf("Your random number is: %f\n",x);
   }
}
