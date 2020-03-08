/* 
************************************************************************
*  eqstring.c: Solution of  wave equation using time stepping          *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: Output data is saved in 3D grid format used by gnuplot     *
************************************************************************
*/
#include <stdio.h>
#include <math.h>

#define rho 0.01      /* density per length */
#define ten 40.0      /* tension */
#define max 100       /* time steps */

main()
{
   int i,k;
   double x[101][3];
   
   FILE *out;       /* save data in string.dat */
   out  = fopen("eqstring.dat","w");
   
   for(i=0; i<81; i++)      /* initial configuration */
   {
      x[i][0] = 0.00125*i; 
   }
   for(i=81; i<101; i++)
   {
      x[i][0] = 0.1-0.005*(i-80);  
   }
   for(i=1; i<100; i++)           /* first time step  */
   {
      x[i][1] = x[i][0]+0.5*(x[i+1][0]+x[i-1][0]-2.0*x[i][0]);        
   }
   
   for(k=1; k<max; k++)                 /* all later time steps  */
   {
      for(i=1; i<100; i++)
      {
         x[i][2] = 2.0*x[i][1]-x[i][0]+(x[i+1][1]+x[i-1][1]-2.0*x[i][1]);
      }
      for(i=0; i<101; i++)
      { 
         x[i][0] = x[i][1];    
         x[i][1] = x[i][2];     
      }
       
      if((k%5) == 0)                      /* print every 5th point */
      {
         for(i=0; i<101; i++) 
         { 
            fprintf(out, "%f\n",x[i][2]); /* gnuplot 3D grid format */
         }
         fprintf(out, "\n");          /* empty line for gnuplot */
      }  
   }
   printf("data stored in eqstring.dat\n");
   fclose(out);
}
