/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
 by RH Landau, MJ Paez, and CC BORDEIANU 
 Copyright Princeton University Press, Princeton, 2008.
 Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
 MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest2007
 Support by National Science Foundation
*/
/* A cellular automaton following the rules:
 old line:	 0 0	1 1	1 0	 0 1
new line	  0	0	1	 1	*/
#include<stdio.h>
#include<stdlib.h>
#include<math.h>
main () {
 int data[30][2]; int i, r, j;
  for ( i = 0; i < 30; i++ ){                                           // First line
   data[i][0] = (int)(drand48()*2);
   printf("%d ",data[i][0]);
 }
 for( j = 0; j < 30; j++){                                               // New lines
   printf("\n");
   data[0][1] = (int)(drand48()*2);                                // 1st cell random
   printf( "%d ",data[0][1] );
   for ( i=1; i<30; i++) {                                       // Rules are applied
     if ( data[i-1][0]==0 && data[i][0]==0 ) data[i][1]=0;
     if ( data[i-1][0]==1 && data[i][0]==1 ) data[i][1]=0;
     if ( data[i-1][0]==0 && data[i][0]==1 ) data[i][1]=1;
     if ( data[i-1][0]==1 && data[i][0]==0 ) data[i][1]=1;
    printf("%d ",data[i][1]);
   }
   for ( i=0; i<30; i++ ) data[i][0]=data[i][1];	                 // New data to old
   } 
printf("\n");
 } 