/* 
************************************************************************
*  limit.c: Determine machine precision e                              *
*           i.e.  the smallest e for which 1 + e .ne. 1               *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                 *
*  related information: the float.h file             *
*  comment: very crude program which produces lots of screen output    *
************************************************************************
*/

#include <stdio.h>

#define N 60       
        
main()
{
   double eps=1.0, one;       /* starting values */
   int i;

   for(i=0; i<N; i++)
   {
      eps  /= 2.;       /* divide by two */
      one=1.0+eps;      
      printf("%.18f \t %.16e \n",one, eps);
   }
}
