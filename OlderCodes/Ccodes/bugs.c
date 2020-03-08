/* 
************************************************************************
*  bugs.c: Bifurcation diagram for logistic map                        *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: plot without conneting datapoints with lines               *
************************************************************************
*/
#include<stdio.h>

#define m_min 0.0                         /* minimum for m  */ 
#define m_max 4.0                         /* maximum for m  */
#define step 0.01                         /* stepsize for m  */

main()
{
   double m, y;
   int i;
   
   FILE *output;        /* save data in bugs.dat */
   output = fopen("bugs.dat","w");
   
   for (m=m_min; m<=m_max; m+=step)       /* loop for m  */
   {
      y  = 0.5;                           /* arbitrary starting value */
      for (i=1; i<=200; i++)              /* ignore transients */
      {
          y = m*y*(1-y);       
      }
      for(i=201; i<=401; i++)             /* then record 200 points  */
      {
         y  = m*y*(1-y);
         fprintf(output, "%.4f\t%.4f\n", m, y);
      }
   }
   printf("data stored in bugs.dat.\n");
   fclose(output);
}
