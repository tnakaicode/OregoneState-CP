/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  ising.c: ising model of magnetic dipole string         
// Plot without conneting datapoints with lines  

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// if you don't have drand48 uncomment the following two lines 
    #define drand48 1.0/RAND_MAX*rand 
    #define srand48 srand        

#define max 100                                  // number of objects
#define kt 100.0                                       // temperature
#define J -1                                       // exchange energy
#define seed 68111                                // seed for srand48

main() {
	
  int i, j, element, array[max];
  double olden, newen;
  double energy(int array[]);                     // energy of system
	
  FILE *output1, *output2;                       // save spin ups and
  output1= fopen("spin-up.dat" ,"w");           // downs in two files
  output2= fopen("spin-down.dat", "w");
  srand48(seed);                                    // seed generator
	                                                   // uniform start
  for (i = 0; i<max; i++) array[i] = 1;   
	                                                       // time loop
  for (i = 0; i<= 500; i++)  {                         
    olden  = energy(array);                         // initial energy
    element= drand48()*max;                       // pick one element
    array[element] *= -1;                              // change spin
    newen  = energy(array);                   // calculate new energy
		                                          // reject/accept change
    if ( (newen>olden) && (exp((-newen + olden)/kt) <=  drand48()) )
      { array[element]= array[element]*(-1);   }  
		                                           // save "map" of spins
    for (j = 0; j<max; j++)   {               
      if (array[j] ==  1) fprintf(output1, "%d  %d\n", i, j);
      if (array[j] == -1) fprintf(output2, "%d  %d\n", i, j);
    }
  }
  fclose (output1);
  fclose (output2);
  printf("data saved in spin-up.dat, spin-down.dat\n");
}                                              // end of main program

double energy (int array[]) {              // function returns energy
	
  int i;
  double sum= 0.;  
  for (i = 0; i<(max-1); i++)                // loop through elements
     sum += array[i]*array[i+1];     
  return (J*sum);
}
