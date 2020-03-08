/* 
************************************************************************
*  diff.c:  Differentiation with forward, central and  extrapolated    *
*            difference methods                                        *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: results saved as x y1 y2 y3                                *
************************************************************************
*/
#include <stdio.h>
#include <math.h>

#define h 1e-5                          /* stepsize for all methods */
#define xmax 7                        /* range for calculation */
#define xmin 0
#define xstep 0.01                  /* stepsize in x  */

main()
{ 
   double dc, result, x;
   double f(double);      /* function we differentiate */ 
          /* see end of code */             
   FILE *output;      /* save data in diff.dat */
   output = fopen("diff.dat","w");
   
   for (x=xmin; x<=xmax; x+=xstep)
   {
      fprintf(output,"%f\t", x);
 
      result=(f(x+h)-f(x))/h;                 /* forward difference */
      fprintf(output, "%.10f\t", result);
   
      result=(f(x+h/2)-f(x-h/2))/h;           /* central difference */
      fprintf(output, "%.10f\t", result);
   
      result=(8*(f(x+h/4.0)-f(x-h/4.0))-(f(x+h/2)-f(x-h/2.0)))/(3.0*h);
      fprintf(output, "%.10f\n", result);     /* extrapolated diff */
   }
   printf("data stored in diff.dat\n");
   fclose(output);
}
/*---------------------end of main program----------------------------*/

/* the function we want to differentiate */
double f(double x)    
{
   return(cos(x));
}
