/*    From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC Bordeianu, Univ Bucharest, 2008
   Support by National Science Foundation
*/
//  fourier.c: Discrete Fourier Transformation        
/*  comment: The program reads its input data from a file in the   
*       same directory called input.dat. This file has to contain   
*           only y(t) values separated by whitespaces which are real
*     The output is the direct output from the algorithm which     
*     will probably look very different than what you are used      
*           to. The output has the form                             
*       frequency index \t real part \t imaginary part              
*  related programs: invfour.c                                     */

#include <stdio.h>
#include <math.h>
#define max 1000      // max number of input data 
#define PI 3.1415926535897932385E0

main()  {
	
  double imag, real,input[max+1];
  int i = 0,j,k;
  FILE *data;
  FILE *output;  
	
	             // read data from input.dat, save data in fourier.dat
  data = fopen("input.dat", "r");              
  output = fopen("fourier.dat", "w");          
	
  while ((fscanf(data, "%lf", &input[i]) != EOF) && (i<max))  i++;
	                                              // loop for frequency  
  for (j = 0; j<i; j++) {            
		                                               // clear variables
    real = imag = 0.0;    
		                                                // loop for sums
    for (k = 0; k<i; k++)  {                              
      real+= input[k]*cos((2*PI*k*j)/i);
      imag+= input[k]*sin((2*PI*k*j)/i);
    }
    fprintf(output, "%d\t%f\t%f\n", j, real/i, imag/i );
   }
   printf("data stored in fourier.dat.\n");
   fclose(data);
   fclose(output);
}    
