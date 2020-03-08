/* 
************************************************************************
*  exp-good.c: A good algorithm for calculating exponential            *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                             
*                                                                      *
*  related programs: exp-bad.c.c                                       *
************************************************************************
*/

#include <stdio.h>
#include <math.h>

#define min 1E-10                           /* limit for accuracy */
#define max 10.                             /* maximum for x */
#define step 0.1                            /* interval */

main ()
{
   double x, sum, element;         
   int n;          

   FILE *output;               /* save results in */
   output=fopen("exp-good.dat", "w");      /* exp-good.dat */
   
   for (x=0.0; x<=max; x+=step)
   {
      sum = element = 1.;                  /* reset variables  */
      n   = 0;
      do                                   /* sum terms until */
      {                                    /* accuracy is reached */
         n++; 
         element *= -x/n;                  /* calculate next element */
         sum += element;
      }while ((sum == 0) || (fabs(element/sum) > min));
      
      fprintf(output, "%f\t%e\n", x, sum);
   }
   printf("results saved in exp-good.dat\n");     
   fclose(output);
}     
