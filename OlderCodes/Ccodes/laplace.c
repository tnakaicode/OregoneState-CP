/* 
************************************************************************
*  laplace.c:  Solution of Laplace equation with finite differences    *
*                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                  *
*  comment: Output data is saved in 3D grid format used by gnuplot     *
************************************************************************
*/
#include <stdio.h>

#define max 40                         /* number of grid points */

main()
{
   double x, p[max][max];
   int i, j, iter, y;
   
   FILE *output;      /* save data in laplace.dat */
   output = fopen("laplace.dat","w");
   
   for(i=0; i<max; i++)                 /* clear the array  */
   {   
      for (j=0; j<max; j++) p[i][j] = 0;
   }

   for(i=0; i<max; i++) p[i][0] = 100.0;        /* p[i][0] = 100 V */   

   for(iter=0; iter<1000; iter++)               /* iterations */
   {
      for(i=1; i<(max-1); i++)                  /* x-direction */
      {
         for(j=1; j<(max-1); j++)               /* y-direction */
         {
            p[i][j] = 0.25*(p[i+1][j]+p[i-1][j]+p[i][j+1]+p[i][j-1]);
         }
      }
   }
   
   for (i=0; i<max ; i++)         /* write data gnuplot 3D format */
   {
      for (j=0; j<max; j++) 
      {
         fprintf(output, "%f\n",p[i][j]);
      }
      fprintf(output, "\n");    /* empty line for gnuplot */
   }
   printf("data stored in laplace.dat\n");
   fclose(output);
}
 
