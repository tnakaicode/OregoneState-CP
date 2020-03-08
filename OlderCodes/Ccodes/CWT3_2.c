/* 
************************************************************************
* CWT3.c Continous Wavelet Transform *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
************************************************************************
*/

#include<stdio.h>
#include<math.h>
#define n 120
#define m 100
#define omega1 1.0//  init & final freq 
#define omega2 5.0
#define tau1 -81.92//  initial tau 
#define l 750
 main() {
 int i, j, o, t; 
 double input[l],c[m][n] ;// input signal
  double WaveletReal( double t);
  double WaveletImag( double t );     
    // amplitude in spectrogram.dat,  signal in input.dat
   
FILE *output1,*output2;
output1=fopen("spectrogram.dat","w");
output2=fopen("input.dat","w");

   for( t=0; t < 750; t++ ) {
    if( t > 0    && t <= 250 ) input[t]=  5.*sin(6.28*t);
    if( t >= 250 && t <= 750 ) input[t] =10.*sin(2.*6.28*t);}
  double tau,dtau, omega, domega, WTreal, WTimag, max;
        // Psi(t)=Psi((t-tau)/s) = Psi((t-tau)*omega) 
        // tau2 = tau1 + n*dtau      translation
        dtau=1.0/m;
        double PsiReal[16834];// real part
        double PsiImag[16834];// imag part
        domega = pow(omega2/omega1, 1.0/m);  // scaling
        omega = omega1;
     for(i=0;i<16834;i++){
       PsiReal[i]=0.0;
       PsiImag[i]=0.0;
     }   
   for (i=0;i<m;i++) // compute daughter wavelet function
        {   tau=tau1;
            for (o=0; o<16834; o++) 
            // for signal files up to 2^13 = 8192 long
            {  PsiReal[o] = WaveletReal( tau*omega );
               PsiImag[o] = WaveletImag( tau*omega );
                tau = tau + dtau;   }            // translation
            for (j=0;j<n;j++)   // compute CWT 
            {   
             WTreal = 0.0;
             WTimag = 0.0;
             for (o=0;o<750;o++) 
              { WTreal += input[o]*PsiReal[8192-(j*750)/n+o];
                WTimag += input[o]*PsiImag[8192-(j*750)/n+o]; 
                }
            c[i][j] = sqrt(WTreal*WTreal+WTimag*WTimag);
            }
            omega = omega*domega; // scaling
            }
  max=0.0001;       
  for (i = 0; i<m; i++) 
    {
        for (j=0; j<n; j++)  //renormalize
        { if(c[i][j]>max)   max=c[i][j];                
        fprintf(output1,"%f\n ",c[i][j]/max); 
        }      
        fprintf(output1,"\n"); 
     } }
//  Morlet wavelet
    double WaveletReal( double t)
    {   double sigma = 4.0;
        return cos(2.0*3.14*t)*
        exp(-1.0*t*t/(2.0*sigma*sigma)) / (sigma*sqrt(2.0*3.14));
    }
    double WaveletImag( double t )
    {   double sigma = 4.0;
        return sin(2.0*3.14*t)*
          exp(-1.0*t*t/(2.0*sigma*sigma)) / (sigma*sqrt(2.0*3.14));
    }     
