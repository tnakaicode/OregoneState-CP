/*
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  unim1d.cpp  Object Oriented Program for uniform motion in 1D       c
c                                                                     c
c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c                                                                      c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
*/      
#include<stdio.h>
#include<stdlib.h>
/*---------------- Um1D Class Definition -----------------------------*/
class Um1D                                              /* class is created  */
{
   private:
     double x00,delt,vx,time;                             /* initial values  */
     int steps;                              /* Time steps to write in file  */
     double x(double tt);                             /* makes x=xo+v* dt */
   public:     
     Um1D(double x0, double dt, double vx0, double tott);
     ~Um1D(void);                       /* Class Constructor and Destructor  */
     void archive();                            /* send x vs t to disk file  */
};
/*---------------- Um1D Constructor and Destructor --------------------------*/
Um1D::Um1D(double x0, double dt, double vx0, double tott)
{
//CONSTRUCTOR Um1D: initializes position, veloc., time, delta t
   x00    = x0;
   delt   = dt;
   vx     = vx0;
   time   = tott;
   steps  = (int)(tott/delt);
}
/*--------------------------------*/
Um1D::~Um1D(void)
{
//DESTRUCTOR: Um1D
   printf("Class Um1D destroyed\n");
}
/*--------------- Um1D Methods ----------------------------------------------*/
double Um1D::x(double tt)
{
// METHOD x: returns X=Xo+dt*v
   return x00+tt*vx;
}
/*--------------------------------*/
void Um1D::archive()
{
// METHOD archive: Produces disk file with X at several time intervals 
   FILE  *pf;
   int i;
   double xx, tt;
  
   pf = fopen("unim1d.dat","w");
   tt = 0.;
   
   for(i = 1 ; i <= steps ; i += 1)
   {
      xx = x(tt);                        /* computes X=Xo+t*v , changing Xo  */
      fprintf(pf,"%f  %f\n", tt, xx);
      tt = tt + delt;
   }
   fclose(pf);
}
/*-------------------- Main Program -----------------------------------------*/
main()
{
   double inix,inivx,dtim,ttotal;
   inix   = 5.;
   dtim   = 0.1;
   inivx  = 10.;
   ttotal = 4.;
                                 /* class constructor. Initial values given  */
   Um1D unimotx(inix,dtim,inivx,ttotal);
                                       /* To obtain disk datafile of y vs x  */
   unimotx.archive();
}
