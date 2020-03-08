/* 
************************************************************************
*  soliton.c: Solves the Kortewg-deVries Equation using a finite       *
*             difference method                                        *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
*  comment: Output data is saved in 3D grid format used by gnuplot     *
************************************************************************
*/
#include <stdio.h>
#include <math.h>

#define ds 0.4        /* delta x */
#define dt 0.1        /* delta t */
#define max 2000      /* time steps */
#define mu 0.1        /* mu from KdeV equation */
#define eps 0.2       /* epsilon from KdeV eq. */ 

main()
{
   int i, j, k;
   double a1,a2,a3, fac, time, u[131][3];
   
   FILE *output;      /* save data in soliton.dat */
   output = fopen("soliton.dat","w");
   
   for(i=0; i<131; i++)     /* initial wave form */
   {
      u[i][0] = 0.5*(1.0 - tanh(0.2*ds*i - 5.0));
   }
   u[0][1]   =1.;                       /* end points */
   u[0][2]   =1.;   
   u[130][1] =0.;  
   u[130][2] =0.;
   
   fac  = mu*dt/(ds*ds*ds);             
   time = dt;
   
   for(i=1; i<130; i++)     /* first time step */
   {
      a1 = eps*dt*(u[i+1][0] + u[i][0] + u[i-1][0]) / (ds*6.0);    
           
      if((i>1) && (i<129))
      { 
           a2 = u[i+2][0] + 2.0*u[i-1][0] - 2.0*u[i+1][0] - u[i-2][0]; 
      }
      else a2 = u[i-1][0] - u[i+1][0];
         
           a3 = u[i+1][0]-u[i-1][0];
      u[i][1] = u[i][0] - a1*a3 - fac*a2/3.;       
   }   
                                 
   
   for(j=1; j<max; j++)     /* all other time steps */
   {   
      time+=dt;
      for(i=1; i<130; i++)
      {
         a1 = eps*dt*(u[i+1][1] + u[i][1] + u[i-1][1]) / (3.0*ds);
              
         if((i>1) && (i<129))
         {
            a2 = u[i+2][1] + 2.0*u[i-1][1] - 2.0*u[i+1][1] - u[i-2][1];           
         }
         else a2 = u[i-1][1] - u[i+1][1]; 
         a3      = u[i+1][1] - u[i-1][1];
         u[i][2] = u[i][0] - a1*a3 - 2.*fac*a2/3.;     
      }

      for(k=0; k<131; k++)    /* move one step ahead */
      {
         u[k][0] = u[k][1];
         u[k][1] = u[k][2];
      }
      
      if((j%200)==0)      /* plot every 200th step */
      {
         for(k=0; k<131; k+=2)
         {
            fprintf(output, "%f\n", u[k][2]);   /* gnuplot 3D format */
         }
         fprintf(output, "\n");         /* empty line for gnuplot */
      }
   } 
   printf("data stored in soliton.dat\n");
   fclose(output);
}
