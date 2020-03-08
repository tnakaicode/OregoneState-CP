/* 
************************************************************************
*  lineq.c: Solve matrix equation Ax=b using LAPACK routine sgesv      *
*                      *
* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*
*  NB: may have to rename sgesv to sgesv_                              *
*                                                                      *
*  comment: LAPACK has to be installed on your system                  *
************************************************************************
*/

#include<stdio.h>
#define size 3                                /* dimension of matrix */

main()
{
   int i, j , c1, c2, pivot[size], ok;
   float A[size][size], b[size], AT[size*size];

   A[0][0] = 3.1; A[0][1] =  1.3; A[0][2] = -5.7;        /* matrix A */
   A[1][0] = 1.0; A[1][1] = -6.9; A[1][2] =  5.8;        
   A[2][0] = 3.4; A[2][1] =  7.2; A[2][2] = -8.8;       

   b[0] = -1.3;                                          /* vector b */
   b[1] = -0.1;
   b[2] =  1.8;
 
   for (i=0; i<size; i++)       /* transform the matrix so */ 
   {                                    /* we can pass it to Fortran */         
     for(j = 0 ; j < size ; j += 1) AT[j + size*i] = A[j][i];           
   }                                                  

   c1 = size;     /* define variable so we can pass pointer */                       
   c2 = 1;              /* to these variables to the routine */    

   sgesv(&c1, &c2, AT, &c1, pivot, b, &c1, &ok);    /* sgesv_ for DEC */   
      /* parameters in the order as they appear in the function call: */
      /* order of matrix A, number of right hand sides (b), matrix A, */
      /* leading dimension of A, array records pivoting, */
      /* result vector b on entry, x on exit, leading dimension of b */
      /* return value =0 for success*/ 
   
   if (!ok)
   {                                             
      for (j=0; j<size; j++) printf("%e\n", b[j]);  /* print x */
   }
   else printf("An error occurred\n");
}
