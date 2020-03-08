/* 
************************************************************************
*  rk4.c: 4th order Runge-Kutta solution for harmonic oscillator       *
*                      *
* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*
************************************************************************
*/
       
#include <stdio.h>

#define N 2                                   /* number of equations */
#define dist 0.1                              /* stepsize */
#define MIN 0.0                               /* minimum x */
#define MAX 10.0                              /* maximum x */
 
main()
{
   void runge4(double x, double y[], double step);
   double f(double x, double y[], int i);
   
   double x, y[N];
   int j;
   
   FILE *output;                              /* save data in rk4.dat */
   output = fopen("rk4.dat","w");
   
   y[0] = 1.0;                                /* initial position  */
   y[1] = 0.0;                                /* initial velocity  */
   
   fprintf(output, "%f\t%f\n", x, y[0]);

   for(x = MIN; x <= MAX ; x += dist)
   {
      runge4(x, y, dist);
      fprintf(output, "%f\t%f\n", x, y[0]);   /* position vs. time */
   }
   printf("data stored in rk4.dat\n");
   fclose(output);
}
/*-----------------------end of main program--------------------------*/

/* Runge-Kutta subroutine */
void runge4(double x, double y[], double step)
{
   double h=step/2.0,                         /* the midpoint */
          t1[N], t2[N], t3[N],                /* temporary storage */
          k1[N], k2[N], k3[N],k4[N];          /* for Runge-Kutta  */
   int i;
 
   for (i=0; i<N; i++) t1[i] = y[i]+0.5*(k1[i]=step*f(x, y, i));
   for (i=0; i<N; i++) t2[i] = y[i]+0.5*(k2[i]=step*f(x+h, t1, i));
   for (i=0; i<N; i++) t3[i] = y[i]+    (k3[i]=step*f(x+h, t2, i));
   for (i=0; i<N; i++) k4[i] =                 step*f(x + step, t3, i);
      
   for (i=0; i<N; i++) y[i] += (k1[i]+2*k2[i]+2*k3[i]+k4[i])/6.0;
}
/*--------------------------------------------------------------------*/

/* definition of equations - this is the harmonic oscillator */
double  f(double x, double y[], int i)
{
   if (i == 0) return(y[1]);               /* RHS of first equation */
   if (i == 1) return(-y[0]);              /* RHS of second equation */
}
