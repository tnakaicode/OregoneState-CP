/* 
************************************************************************
*  over.c: Determine overflow and underflow limits                     *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  related information: the float.h file                               *
*  comment: very crude program which produces lots of screen output    *
************************************************************************
*/

#include <stdio.h>

#define N 1024      /* might not be big enough to cause */ 
        /* over and underflow */
main()
{
   double under=1., over=1.;      /* starting values */
   int i;

   for(i=0; i<1024; i++)
   {
      under /= 2.;        /* divide by two */
      over  *= 2.;        /* multiply by two */
      printf("%d. under: %e over: %e \n",i+1,under,over);
   }
}
