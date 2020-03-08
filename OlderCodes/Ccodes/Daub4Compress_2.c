/* 
************************************************************************
*  Daub4Compress.c
  1D Daubechies 4-coefficient wavelet transform based on Press et al   *
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
#include<stdlib.h>
 main() {
// initial data in initial.dat, compressed image in daubc.dat
 
FILE *output1,*output2;
      output1=fopen("initial.dat","w");
      output2=fopen("daubc.dat","w");

int i;
int n=1024;
void wt1(double x[], int n, int sign);
void daub4(double x[], int n, int sign);
double x[1025];//data vector
//initial data via  arbitrary function
for (i=1;i<=n;i++)
    { x[i] = (1.0/1204)* (i*i-3.0*i); 
      fprintf(output1,"%d\t%f\n",i,x[i]); }
wt1(x,n,1);   //DWT
wt1(x,n,-1);   //inverse DWT
for (i=1; i <= n; i++) fprintf(output2,"%d\t%f\n",i,x[i]);
}
void wt1(double x[], int n, int sign)
//1D DWT via pyramid algorithm, replace x[] by TF
//sign = +- 1 for TF/inverse, n = 2^N, nn>>1 = nn/2
{
void daub4(double x[], int n, int sign);
int nn;
if (n < 4) return;
if (sign >= 0) {for (nn=n;nn>=4;nn>>=1) daub4(x,nn,sign);}
else { for (nn=4;nn<=n;nn<<=1) daub4(x,nn,sign); }
}
void daub4(double x[], int n, int sign)
//Apply 4-coefficient matrix, or transpose for sign=-1
{       //Daubechies 4 coefficients
double C0 =0.4829629131445341;  
double C1 =0.8365163037378079;
double C2 =0.2241438680420134;
double C3 =-0.1294095225512604;
double temp[1025];      
int nh,nh1,i,j;
double tol;             // threshold
tol=(40.0/100.0)*868.0;
if (n < 4) return;
nh1=(nh=n >> 1)+1;  // L and H filters
if (sign >= 0) {for (i=1,j=1;j<=n-3;j+=2,i++) 
{ temp[i]= C0*x[j]+C1*x[j+1]+C2*x[j+2]+C3*x[j+3]; 
  temp[i+nh] = C3*x[j]-C2*x[j+1]+C1*x[j+2]-C0*x[j+3]; 
  if (abs(temp[i+nh]) < tol)  temp[i+nh] = 0.0;  }
temp[i] = C0*x[n-1]+C1*x[n]+C2*x[1]+C3*x[2]; 
temp[i+nh] = C3*x[n-1]-C2*x[n]+C1*x[1]-C0*x[2]; 
if (abs(temp[i+nh]) < tol) temp[i+nh] = 0.0;
} 
else //inverse DWT
{ temp[1] = C2*x[nh]+C1*x[n]+C0*x[1]+C3*x[nh1]; 
  temp[2] = C3*x[nh]-C0*x[n]+C1*x[1]-C2*x[nh1]; 
  for (i=1,j=3;i<nh;i++) {
temp[j++] = C2*x[i]+C1*x[i+nh]+C0*x[i+1]+C3*x[i+nh1]; 
temp[j++] = C3*x[i]-C0*x[i+nh]+C1*x[i+1]-C2*x[i+nh1]; }
}
for (i=1;i<=n;i++) { 
         x[i]=temp[i];
}
}  //main
