
/*-------------------------------------------------------
SplineAppl.c  Cubic Spline fit to data, based on Press et al
input array x[n], y[n] represents tabulation function y(x)
with x0 < x1 ... < x(n-1). n = # of tabulated points
output yout for given xout (here xout via loop at end)
yp1 and ypn: 1st derivatives at endpoints, evaluated internally
y2[n]  is array of second derivatives
(setting yp1 or  ypn >0.99e30 produces natural spline)
From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
by RH Landau, MJ Paez, and CC BORDEIANU 
Copyright Princeton University Press, Princeton, 2008.
Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                             
 -------------------------------------------------------
*/

#include<stdio.h>
#include<math.h>
#define n 9
#define np 15
#define Nfit 3000 // enter the desired number of points to fit

       
 main()

 {	// save data in Spline.dat
 FILE *output1,*output2;
 output1=fopen("Spline.dat","w");
 output2=fopen("Input.dat","w");

    // input data 
    //you may enter your own data here!
//double x[] = {0., 1.2, 2.5, 3.7, 5., 6.2, 7.5, 8.7, 9.9}; 
//double y[] = {0., 0.93, 0.6, -0.53, -0.96, -0.08, 0.94, 0.66, -0.46}; 
double x[] = {0., 0.12, 0.25, 0.37, 0.5, 0.62, 0.75, 0.87, 0.99}; 
double y[] = {10.6, 16.0, 45.0, 83.5, 52.8, 19.9, 10.8, 8.25, 4.7}; 
double xout, yout; 
double y2[9];
int i;
int klo,khi,k ;
double h,b,a;
double p,qn,sig,un,yp1,ypn;
double u[n];
// save input data in Input.dat file
for(i=0;i<n;i++){
fprintf (output2,"%f\t%f\n",x[i],y[i]);
}

yp1  =  (y[1]-y[0])/(x[1]-x[0])  // 1st deriv at initial point
    - (y[2]-y[1])/(x[2]-x[1]) 
    + (y[2]-y[0])/(x[2]-x[0]);
ypn = (y[n-1]-y[n-2])/(x[n-1]-x[n-2]) // 1st deriv at end point
    - (y[n-2]-y[n-3])/(x[n-2]-x[n-3])
     +(y[n-1]-y[n-3])/(x[n-1]-x[n-3]);

if (yp1 > 0.99e30) y2[0] = u[0] = 0.0; //natural spline test
    else {
    y2[0]  =  (-0.5);
    u[0] = (3.0/(x[1]-x[0]))*((y[1]-y[0])/(x[1]-x[0])-yp1);
         }
for (i=1; i<=n-2; i++)   // decomposition loop; y2, u are temps
    {
    sig = (x[i]-x[i-1])/(x[i+1]-x[i-1]);
    p = sig*y2[i-1]+2.0;
    y2[i] = (sig-1.0)/p;
    u[i] = (y[i+1]-y[i])/(x[i+1]-x[i]) - (y[i]-y[i-1])/(x[i]-x[i-1]);
    u[i] = (6.0*u[i]/(x[i+1]-x[i-1])-sig*u[i-1])/p;
    }
if (ypn > 0.99e30) qn = un = 0.0;   //test for natural
else                       // else evaluate second derivative
     { qn = 0.5;
       un = ((3.0)/(x[n-1]-x[n-2]))*(ypn-(y[n-1]-y[n-2])/(x[n-1]-x[n-2]));
     }
y2[n-1] = (un-qn*u[n-2])/(qn*y2[n-2]+1.0);
for (k = n-2;k>= 0;k--) {y2[k] = y2[k]*y2[k+1]+u[k];}    // back substitution
        // splint (initialization) ends

    // Parameters determined, Begin *spline* fit
for( i=1; i<= Nfit; i++ )  // loop over xout values
    {
    xout = x[0] + (x[n-1]-x[0])*(i-1)/(Nfit);
    
    klo = 0;                //Bisection algor to find place in table
    khi = n-1;              // klo, khi bracket xout value 
    while (khi-klo > 1) {
        k = (khi+klo) >> 1;
        if (x[k] > xout) khi = k;
        else klo = k;     }
    h = x[khi]-x[klo];
    if (x[k] > xout) khi = k;
    else klo = k;
    h = x[khi]-x[klo];
    a = (x[khi]-xout)/h;
    b = (xout-x[klo])/h;
    yout = (a*y[klo]+b*y[khi]+((a*a*a-a)*y2[klo]
                   +(b*b*b-b)*y2[khi])*(h*h)/6.0);
    /* write data using gnuplot 2D format */
    fprintf (output1,"%f\t%f\n",xout,yout);  
    
    }
fclose(output1);
fclose(output2);
printf("data stored in Spline.dat"); 
}


