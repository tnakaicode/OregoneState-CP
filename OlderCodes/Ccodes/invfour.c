/* 
************************************************************************
*  invfour.c: Inverse Discrete Fourier Transformation                  *
*                      *
c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c
*  comment: The program reads its input data from a file in the        *
*       same directory called fourier.dat which must have the      *
*     format                   *
*   frequency index \t real part \t imaginary part         *
*     as created with the program fourier.c          *
*     The output has the same format.                *    
*  related programs: fourier.c                 *
************************************************************************
*/
#include <stdio.h>
#include <math.h>

#define max 10000
#define PI 3.1415926535897932385E0
 
main()
{
double imag, real, input [2][max];
int i=0,j,k;
 
FILE *data;
FILE *output;       
data=fopen("fourier.dat", "r");   /* read data from fourier.dat */  
output=fopen("invers.dat", "w");  /* save data in invers.dat */ 

while (fscanf(data, "%d %lf %lf", &j, &input[0][i], &input[1][i]) !=EOF)
{     
    i++;    /* input[0][x]:real, input[1][x]:imaginary */
}
 
for (j=0; j<i; j++)     /*loop for the frequency index*/
{
   real=imag=0.0;     /* clear variables */
   for (k=0; k<i; k++)      /*loop for sum*/
   {
      real+=input[0][k]*cos(2*PI*k*j/i)+input[1][k]*sin(2*PI*k*j/i);
      imag+=input[1][k]*cos(2*PI*k*j/i)-input[0][k]*sin(2*PI*k*j/i);
   }
   fprintf(output, "%i\t %f\t%f\n", j, real, imag);
}
printf("data saved in invers.dat\n");
fclose(output);
fclose(data);
}    
