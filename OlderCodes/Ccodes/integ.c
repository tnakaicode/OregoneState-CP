/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//   integ.c:  Integration using trapezoid, Simpson and Gauss rules
/* comment: The derivation from theoretical result for each method  
            is saved in x y1 y2 y3 format.            
            Program needs gauss.c in the same directory.         */  
#include <stdio.h>
#include <math.h>
#include "gauss.c"                   // returns Legendre pts, weights
#define max_in  501                        // max number of intervals
#define vmin 0.0                             // ranges of integration
#define vmax 1.0      
#define ME 2.7182818284590452354E0                  // Euler's number

main()  {
	
  int i;      
  float result;
  float f(float x);               
  float trapez  (int no, float min, float max);     // trapezoid rule
  float simpson (int no, float min, float max);     // Simpson's rule
  float gaussint(int no, float min, float max);        // Gauss' rule
  
  FILE *output;                             // save data in integ.dat
  output= fopen("integ.dat","w");
	                                          // Simpson requires odd N
  for (i = 3; i<= max_in; i+= 2) {                
    result= trapez(i, vmin, vmax);
    fprintf(output, "%i\t%e\t", i, fabs(result-1+1/ME));
    result= simpson(i, vmin, vmax);
    fprintf(output, "%e\t", fabs(result-1+1/ME));
    result= gaussint(i, vmin, vmax);
    fprintf(output, "%e\n", fabs(result-1+1/ME));
  }
  printf("data stored in integ.dat\n");
  fclose(output);
}                                                              // end  

float f (float x)                            // function to integrate
{ return (exp(-x));  }

float trapez (int no, float min, float max) {            
	                                                 // trapezoid rule
  int n;       
  float interval, sum = 0., x;    
  interval= ((max-min) / (no-1));
  for (n = 2; n<no; n++)   {                     // sum the midpoints
    x   = interval * (n-1);      
    sum += f(x)*interval;
  }
  sum += 0.5 *(f(min) + f(max)) * interval;      // add the endpoints
  return (sum);
}      

float simpson (int no, float min, float max) {           
	                                                  // Simpson's rule
  int n;        
  float interval, sum = 0., x;
  interval= ((max -min) /(no-1));
  for (n = 2; n<no; n+= 2) {                   // loop for odd points
    x= interval * (n-1);
    sum += 4 * f(x);
  }
  for (n = 3; n<no; n+= 2)   {                // loop for even points
    x= interval * (n-1);
    sum += 2 * f(x);
  }   
  sum +=  f(min) + f(max);                // add first and last value
  sum *= interval/3.;             
  return (sum);
}  


float gaussint (int no, float min, float max) {        // Gauss' rule
  int n;        
  float quadra= 0.;
  double w[1000], x[1000]; 
  void gauss(int npts, int job, double a, double b, double x[], double w[]);                       // for points and weights
  gauss (no, 0, min, max, x, w);       // Gauss Legendre points & wts
  for (n = 0; n< no; n++) quadra += f(x[n])*w[n];    // calc integral
  return (quadra);                  
}
