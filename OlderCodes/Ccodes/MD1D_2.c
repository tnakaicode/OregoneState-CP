/* 
************************************************************************
* MD1D.c Molecular dynamics for n atoms in 1D with  Lennard-Jones potential,
  velocity Verlet algorithm                                            *
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
-----------------------------------------------------------------------------------
* Natom - number atoms            Nmax - maximum number of atoms  
* Nstep - number time steps       Nprint - number steps between printing
* L   = box size                  h - time step 
* hover2 = h/2                    PE - potential energy  
* KE - kinetic energy             T - temperature
* fx[] - force                    x[] - positions
* vx[] - velocity                 Ndim - dimension of box (1D, 2D, 3D)
-----------------------------------------------------------------------------------
*/
#include<stdio.h>
#include<math.h>
#include<stdlib.h>
#define Natom 8
#define Nmax 513 
    
  //Class variables used by Forces Method

 main()     //Main Method
{
  double x[8], fx[8][2];
 int  t1, t2, i, Itemp, t,  Nstep=10000, Nprint=100, Ndim=1,L;
 double h=0.004, hover2,  PE, KE,T,Tinit=10.0; 
 double vx[8]; 
 L = floor(pow(1.*Natom, 1./Ndim)); 
 //Natom =  (int)pow(L, Ndim);
double Forces(int t, double PE, double x[8],double fx[8][2]);
 printf("Natom = %d\tL= %d\n",Natom,L);
       i = -1;
//Set up initial lattice configuration, fits within box of side L    
       for(int ix = 0; ix<=L-1; ix++){
       i = i+1;
       x[i] = ix;
printf("init x = %f\n",x[i]);
//Initial velocities according to Maxwell-Gaussian Distribution
        vx[i] =((rand()+rand()+rand()+
                   rand()+rand()+rand()+
                   rand()+rand()+rand()+
                   rand()+rand()+rand())/(12.0*RAND_MAX))-0.5; 
        vx[i] = vx[i]*sqrt(Tinit); //scale velocity with temperature
  printf("init vx = %f\n",vx[i]);
 } 
 //t, t+h indices
 t1 = 0;
 t2 = 1;
 hover2 = h/2.0;
 // initial KE & PE v 
 t = 0;
 KE = 0.0;
 PE = 0.0;
 PE = Forces(t1, PE,x,fx);
 for(i = 0; i <= Natom-1; i++) KE=KE+(vx[i]*vx[i])/2.0;
printf("%d\tPE =%f\tKE =%f\tPE+KE =%f\n",t,PE,KE,PE+KE);
//Main loop
     for( t = 1; t < Nstep; t++ ){ 
     for( i = 0; i <= Natom-1; i++ ){ 
     // velocity Verlet algorithm
         PE = Forces(t1, PE,x,fx);
         x[i] = x[i] + h*(vx[i] + hover2*fx[i][t1]);
      //periodic boundary conditions
      if (x[i] <= 0.) x[i] = x[i] + L;
      if (x[i] >= L)  x[i] = x[i] - L;
    }
     PE = Forces(t2, PE,x,fx);
     KE = 0.; 
     for( i = 0; i <= Natom-1; i++){ 
            vx[i] = vx[i] + hover2*(fx[i][t1] + fx[i][t2]);
            KE = KE + (vx[i]*vx[i])/2; }
     T = 2.*KE / (3.*Natom); 
     if (t%Nprint == 0) 
            {printf("%d\tPE =%f\tKE = %f \tPE+KE =%f \n",t,PE,KE,PE+KE);
        Itemp = t1; //time t and t+h
        t1 = t2;
        t2 = Itemp;
    }
}
}
/*************************************************************
*   Forces function
*    Compute forces, PE
*    return PE as function value, Forces via class variables
*        V(r) = 4*(1/r**12 - 1/r**6)
**************************************************************/
double Forces(int t, double PE,double x[8],double fx[8][2])
{   
int i, j;
int L,Ndim=1;      

double fijx;
double r2, invr2=0, dx, r2cut;
r2cut = 9.;// Cut-off radius
L = floor(pow(1.*Natom, 1./Ndim)); 
double  sign(double a,double b);
// Initialize forces.
 PE = 0.;
 for (i=0; i<= Natom-1; i++) {fx[i][t] = 0.;}
//Compute forces.
      for(i = 0; i<= Natom-1; i++){
          for(j = i+1; j<Natom; j++){
  dx = x[i]-x[j];
//minimum image criterium
   if(fabs(dx) > 0.50*L) {dx = dx - sign(L,dx);}
   r2 = pow(dx,2);
// Cut off
   if(r2 < r2cut){
       if(r2==0.) r2=0.0001;
       invr2 = 1./r2;   
       fijx =  48.*(pow(invr2,3)-0.5)*pow(invr2,3);    
       fijx = fijx*invr2*dx;
       fx[i][t] = fx[i][t] + fijx;
       fx[j][t] = fx[j][t] - fijx;
       PE = PE + 4*pow(invr2,3)*( pow(invr2,3) - 1.);
      }
    }
   }
       return PE;    
}
double  sign(double a,double b){
    if (b >= 0.) return abs(a);
    else  return -abs(a); }      

