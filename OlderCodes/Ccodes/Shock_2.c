/* 
************************************************************************
*  Shock.c Solve the Burger's shock equation using Lax-Wendroff scheme *
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
#define m 100  //number of grid points
   main(){
      FILE *output1,*output2;
      output1=fopen("numerical.dat","w");// save numerical solution data in  numerical.dat 
      output2=fopen("initial.dat","w"); // save initial condition in   initial.dat 
      double u[m];//final solution
      double u0[m];//initial data
      double epsilon = 1.0; //wave speed   
      double beta=0.1;
      //CFL number (beta = epsilon*dt/dx)  
      double x,dx,dt,T_final;
      int i,j,n;
      dx=2.0/m;// space step
      dt=beta*dx/epsilon; // time step
      T_final=0.15;
      n=(int)(T_final/dt);
      //initial data
      for(i=0;i<m-1;i++){
        x = i*dx;
        u0[i] = 3.0*sin(3.2*x);
        fprintf(output2,"%f\t%f\n",0.01*i,u0[i]); // save initial data
     }
     //Lax-Wendroff scheme
     for (j=1;j<n;j++){ 
       for (i=0;i<m-3;i++){
         u[i+1] = u0[i+1]-
         (pow(u0[i+2],2)-pow(u0[i],2))*(0.25*beta)+
         (((u0[i+2]+u0[i+1])/2.0)*(pow(u0[i+2],2)-pow(u0[i+1],2))- 
         ((u0[i+1]+u0[i])/2.0)*(pow(u0[i+1],2)-pow(u0[i],2)))*0.25*beta*beta;
         u[0]=0.0; u[m-1]=0.0;
         if((j%5==0)&&(i%4==0))  fprintf(output1,"%f\n ",u[i]);
         u0[i] = u[i]; //shift new to old
      }
      if(j%5==0)fprintf(output1,"\n");
    }
   
    printf("Numerical solution data saved in numerical.dat");
  }//main
