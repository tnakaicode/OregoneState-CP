/* 
************************************************************************
*  rk45_rhl.c Ordinary Differential Equations Solver (ODES).
  Solve the differential equation  using Runge-Kutta-Fehlberg Method
  with variable step size.                                             *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
************************************************************************

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

main() 
{// open  file rk45_rhl.dat for output data
FILE *out1;
out1=fopen("rk45_rhl.dat","w");


    

double h,t,s,s1,hmin,hmax;

double y[2];
double fReturn[2]; 
double ydumb[2];
double k1[2]; double k2[2]; double k3[2];
double k4[2]; double k5[2]; double k6[2];
double err[2]; 

double Tol = 1.0E-8;                    //error control tolerance
double a = 0.0;                        //endpoints
double b = 10.0;
int i,j,n=20;
void f(double t, double y[], double fReturn[]);
y[0]=1.0;  y[1] = 0.0; 
//y[0]=1.0;  y[1] = 0.0;                   //initialize
h = (b-a)/n;                             //tentative number of steps
hmin=h/64;
hmax=h*64;                   //minimum and maximum step size
t = a;
j = 0;

//RHL add of printout for initial step
    
    fprintf(out1,"%f\t%f\t%f\n",t,y[0],y[1]);//output answer to file
   

while (t<b)
{
    if( (t + h) > b ) h = b - t; // the last step

     
    f(t, y, fReturn);  // evaluate both RHS's and return in fReturn
    k1[0] = h*fReturn[0]; //compute the function values
    k1[1] = h*fReturn[1];
   
    for (i=0;i<=1;i++) ydumb[i] = y[i] + k1[i]/4;
    f(t + h/4, ydumb, fReturn);
    k2[0] = h*fReturn[0];
    k2[1] = h*fReturn[1];
    
    for (i=0;i<=1;i++) ydumb[i] = y[i] + 3*k1[i]/32 + 9*k2[i]/32;
    f(t + 3*h/8, ydumb, fReturn);
    k3[0]= h*fReturn[0]; 
    k3[1] = h*fReturn[1];

    for (i=0;i<=1;i++) ydumb[i] = y[i] + 1932*k1[i]/2197
            -7200*k2[i]/2197. +7296*k3[i]/2197;
    f(t+ 12*h/13, ydumb, fReturn);
    k4[0] = h*fReturn[0];  
    k4[1] = h*fReturn[1];  

    for (i=0;i<=1;i++) ydumb[i] = y[i] +439*k1[i]/216 -8*k2[i]
                                  +3680*k3[i]/513 -845*k4[i]/4104;
    f(t+h, ydumb, fReturn);
    k5[0] = h*fReturn[0];  
    k5[1] = h*fReturn[1];  

   for (i=0;i<=1;i++) ydumb[i] = y[i] -8*k1[i]/27 +2*k2[i]
                  -3544*k3[i]/2565 +1859*k4[i]/4104 -11*k5[i]/40;
    f(t+ h/2, ydumb, fReturn);
    k6[0] = h*fReturn[0];  
    k6[1] = h*fReturn[1];

    //zk+1-yk+1
    for (i=0;i<=1;i++) err[i] =abs( k1[i]/360 - 128*k3[i]/4275 - 2197*k4[i]/75240
         + k5[i]/50.0 + 2*k6[i]/55 );
    
    if ((err[0]<Tol)||(err[1]<Tol)||(h<=2*hmin))//accept the approximation
   //if ((err[0]<Tol)||(h<=2*hmin))//accept the approximation
    {//RK4
        for (i=0;i<=1;i++) y[i] = y[i] + 25*k1[i]/216. + 1408*k3[i]/2565.
        + 2197*k4[i]/4104. - k5[i]/5.;
        
        t = t + h;
        
        
        
        j++;}

    if (( err[0]==0)||(err[1]==0)) s=0; //trap division by 0
    else s=0.84*pow(Tol*h/err[0],0.25);//step size scalar

    if ( (s < 0.75) && (h > 2*hmin) )   h /=  2.;  //reduce step
    else if ( (s > 1.5) && (2* h < hmax) ) h *=  2.;//increase step


 fprintf(out1,"%f\t%f\t%f\n",t,y[0],y[1]);//output answer to file

//w.println(" " +t+" "+Math.log(error));
//w.println(" " +t+" "+Math.abs(error));


}
 

//End of while loop


}
//definition of equation-example
//damped harmonic oscillator with harmonic driver
// x" + 100*x + 2*x'=sin 3*t
// we define y[0] = x, y[1] = x'
//  You need to transform the second order differential equation
// in a system of two differential equatios of first order:
// f[0] = x'= y[1]
// f[1] = x" = -100*x-2*x' = -100 *y[0] -2*y[1] + sin 3*t
// You may enter your own equation here!

void f(double t, double y[], double fReturn[])
{
    //fReturn[0] = y[1];  // RHS of first equation
    //fReturn[1] = -y[0]; // RHS of second equation
    fReturn[0]=y[1];
    fReturn[1]=-6.0*pow(y[0],5.0);
    
    return;
}

