/* 
************************************************************************
*  numerov.c: Numerov method to find eigenvalues and eigenfunctions of *
*       a particle in a potential well. As written this program        *
*       only finds odd eigenfunctions correctly and fails for          *
*       even eigenfunctions and if there is no eigenvalue in the       *
*             specified range. The final function is not normalized.   *
*                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              

************************************************************************
*/
#include <stdio.h>
#include <math.h>

#define steps 1000         /* half width of potential well */
#define V -0.001         /* depth of potential well */
#define eps 1E-8         /* accuracy for eigenvalues */
#define Emin -0.001        /* set to find the third */
#define Emax -0.00085        /* eigenfunction */

double k2(int i, double E);      /* returns potential at x */
double diff(double E);         /* difference of derivatives */
void plot(double E);               /* data for final plot */ 

main()
{
   double E, min, max;
   int i=0;          /* counter for iterations */
   min=Emin;        
   max=Emax;
  
   do
   {
      i++;  
      E=(max+min)/2.0;         /* divide energy range */
      if (diff(max)*diff(E)>0) max=E;   /* the bisection algorithm */
      else min=E;
   }while(fabs(diff(E))>eps);
   
   printf("eigenvalue E=%.10f\n", E);
   printf("after %d iterations\n", i);
   plot(E);       
}


/*----------------------- end of main program ------------------------*/ 

/* function returns difference between left and right wavefunction */
double diff(double E)
{
   double one, two, three, plus, minus;
   int i;
   
   one=0.0;
   two=0.00001;
   for (i=1; i<=1500; i++)    /* left side first */
   {
      three=(2*two*(1-5./12.*k2(i,E))-
          (1.+1./12.*k2(i-1,E))*one)/(1.+1./12.*k2(i+1,E));
      one=two;
      two=three;
   }
   minus=two;       /* value at matching point */ 

   one=0.0;       /* reset starting conditions */
   two=0.00001;
   for (i=1; i<500; i++)    /* now the right side */
   {
      three=(2*two*(1.-5./12.*k2(i,E))-
          (1.+1./12.*k2(i+1,E))*one)/(1.+1./12.*k2(i-1,E));
      one=two;
      two=three;
   }
   plus=two;              /* value at matching point */
   return((minus-plus));
}
/*--------------------------------------------------------------------*/

/* function returns k-vector at depending on position i */
double k2(int i, double E)    
{
   if (i<500) return(E);    /* outside the well */
   if (i>=500) return (E-(V));    /* inside the well */
}
/*--------------------------------------------------------------------*/

/* write data for eigenfuntion into files left.dat, right.dat */
void plot(double E)     
{         
   double one, two, three;
   int i;
   
   FILE *right, *left;      /* save data in files */
   right=fopen("right.dat", "w");
   left=fopen("left.dat", "w");
   
   one=0.0;
   two=0.00001;
   for (i=1; i<=1500; i++)    /* left side first */
   {
      three=(2*two*(1-5./12.*k2(i,E))-
          (1+1./12.*k2(i-1,E))*one)/(1+1./12.*k2(i+1,E));
      fprintf(left, "%d\t%f\n", i-1000, three/2.8);
      
      one=two;        
      two=three;
   }
   
   one=0.0;       /* reset starting conditions */
   two=0.00001;
   for (i=1; i<500; i++)    /* now the right side */
   {
      three=(2*two*(1.-5./12.*k2(i,E))-
          (1+1./12.*k2(i+1,E))*one)/(1.+1./12.*k2(i-1,E));
      fprintf(right, "%d\t%f\n", 1000-i, three/2.8);
      
      one=two;
      two=three;
   }
   fclose(left);
   fclose(right);
   printf("data saved in left.dat and right.dat\n");
}
   
