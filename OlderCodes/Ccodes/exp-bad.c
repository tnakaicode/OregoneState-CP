/*    From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC Bordeianu, Univ Bucharest, 2008
   Support by National Science Foundation
*/
//  exp-bad.c: A bad algorithm for calculating exponential 
// related programs: exp-good.c   

#include <stdio.h>     
#include <math.h>               
#define min 1E-10                               // limit for accuracy
#define max 10                                       // maximum for x
#define step 0.1                                         // intervals

main ()   {
	
   double sum, x, up, down;          
   int i,j;
	 
   FILE *output;                                   // save results in
   output = fopen("exp-bad.dat", "w");                 // exp-bad.dat
	                                                  // step through x
   for (x = 0.0; x<= max; x+= step)  {              
     sum= 1.;                                       // reset variables
     i  = 0;
		                                      // sum til accuracy reached
     do   {                                    
       i++;
       up= down= 1;                                // reset variables
       for (j = 1; j<= i; j++)  {
         up   *= -x;                                     // numerator
         down *= j;                                    // denominator
       }
      sum += up/down;
    }
	  while( (sum ==  0) || ((fabs ((up/down)/sum)) > min) );
    fprintf(output, "%f\t%e\n", x, sum);   
   }
   printf("results saved in exp-bad.dat\n");     
   fclose(output);
}      
