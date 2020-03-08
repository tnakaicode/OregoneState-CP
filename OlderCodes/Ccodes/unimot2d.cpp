/*
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  unimot2d.cpp   Object Oriented Program for uniform motion in 2D    c
c                                                                     c
c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c                                                                     c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
*/
#include <stdio.h>
#include <stdlib.h>
/*---------------- Um1D Class Definition ------------------------------------*/
class Um1D                                              /* class is created  */
{
   protected:                    /* so children classes may access the data  */
      double delt;
      int steps;                             /* Time steps to write in file  */
      double x(double tt);                               /* makes x=xo+v*dt  */
   private:
      double x00, vx, time;
   public:     
      Um1D(double x0, double dt, double vx0, double tott);
      ~Um1D(void);                      /* Class Constructor and Destructor  */
      void archive();                           /* send x vs t to disk file  */
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
   return (x00 + tt*vx);
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
/*-------------------------------------*/
/*---------------- Um2D Class Definition ------------------------------------*/
class Um2D : public Um1D
{
// CLASS Um2D: child class of UmD1, for 2 dimensional uniform motion
   protected: 
      double y(double tt);
   private:
      double y00, vy;
   public:
      Um2D(double x0,double dt,double vx0,double tott,
           double y0,double vy0);          /* constructor or the Um2D class  */
      ~Um2D(void);                          /* destructor of the Um2D class  */
      void archive();      /* override Um1D definition because of 2D output  */
};
/*---------------- Um2D Constructor and Destructor --------------------------*/
Um2D::Um2D(double x0,double dt,double vx0,double tott,
           double y0,double vy0):
      Um1D(x0,dt,vx0,tott)
{
// CONSTRUCTOR Um2D: initializes y position and y velocity
   y00   = y0;
   vy    = vy0;
}
/*-------------------------------------*/
Um2D::~Um2D(void)
{
// DESTRUCTOR Um2D
   printf("Class Um2D is destroyed\n");
}
/*---------------- Um2D Methods ---------------------------------------------*/
double Um2D::y(double tt)
{
// METHOD ypos: returns Y=Y0+dt*v
   return (y00 + tt*vy);
}
/*-------------------------------------*/
void Um2D::archive()
{
// METHOD archive: override of Um1D.archive, for 2-dim uniform motion
   FILE  *pf;
   int i;
   double xx, yy, tt;
   
   pf = fopen("unimot2d.dat","w");
   tt = 0.;
   for(i = 1 ; i <= steps ; i += 1)
   {
      xx = x(tt);
      yy = y(tt);
      fprintf(pf,"%f  %f\n",yy,xx);                  /* data is now x vs. y  */
      tt = tt + delt;
   }
   fclose(pf);
}
/*-------------------------------------*/
/*---------------- Main Program ---------------------------------------------*/
 main()
{
   double inix,iniy,inivx,inivy,dtim,ttotal;
   inix   = 5.;
   dtim   = 0.1;
   inivx  = 10.;
   ttotal = 4.;
   iniy   = 3.;
   inivy  = 8.;
                                 /* class constructor. Initial values given  */
   Um2D unimotxy(inix,dtim,inivx,ttotal,iniy,inivy);
   unimotxy.archive();            /* activate desired portion of the object  */
}
