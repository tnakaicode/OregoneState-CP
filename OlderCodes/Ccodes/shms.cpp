/*
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  shms.cpp: Object Oriented Program for simple harmonic motion        c
c            creates Lissajous figures                                 c
c                                                                      c
* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                     c
c  UNIX (DEC OSF, IBM AIX): cpp shms.cpp                              c
c                                                                     c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*---------------- ShmX Class Definition ------------------------------------*/
class ShmX
{
// CLASS ShmX: simple harmonic motion in the x direction
   protected:
      double xpos(double partt);                     /* makes  Ax sin(wt+d)  */
      double delttx;
      int stepsx;                            /* Time steps to write in file  */
   private:
      double Ampx,deltx,omegx,timex;
   public:
      ShmX(double Axini,double delxini,double omegxini, 
           double totaltx,double dtx);
      ~ShmX(void);                      /* Class Constructor and Destructor  */
      void archive();                           /* send x vs t to disk file  */
};
/*----------------ShmX Constructor and Destructor ---------------------------*/
ShmX::ShmX(double Axini,double delxini,double omegxini,
           double totaltx,double dtx)
{
// CONSTUCTOR ShmX: Initializes Amplitude, step sizes, and frequency
   Ampx   = Axini;
   deltx  = delxini;
   omegx  = omegxini;
   timex  = totaltx;
   delttx = dtx;
   stepsx = (int)(totaltx/delttx);
}
/*-------------------------------------*/
ShmX::~ShmX(void)
{
// DESTRUCTOR ShmX
  printf("Class ShmX destroyed\n");
}
/*--------------- ShmX Methods ----------------------------------------------*/
double ShmX::xpos(double partt)
{
// METHOD xpos: returns X=A sin(xt+d)
   return (Ampx*sin(omegx*partt + deltx));
}
/*-------------------------------------*/
void ShmX::archive()
{
// METHOD archive: Produces disk file with X at several time intervals
   FILE  *pf;
   int i;
   double xx,tt;
   
   pf = fopen("cshmx.dat","w");
   tt = 0.;
   for(i = 1 ; i <= stepsx ; i += 1)
   {
      xx = xpos(tt);
      fprintf(pf,"%f  %f\n",tt,xx);
      tt = tt + delttx;
   }
   fclose(pf);
}
/*-------------------------------------*/
/*---------------- ShmY Class Definition ------------------------------------*/
class ShmY
{
// CLASS ShmY: simple harmonic motion in Y direction
   protected:
      double ypos(double partt);                      /* makes Ay sin(wt+d)  */
      double deltty;
      int stepsy;                            /* Time steps to write in file  */
   private:
     double Ampy, delty, omegy, timey;
   public:
      ShmY(double Ayini, double delyini, double omegyini,
           double totalty, double dty);
      ~ShmY(void);                      /* Class Constructor and Destructor  */
      void archive();                           /* send y vs t to disk file  */
};
/*---------------- ShmY Constructor and Destructor --------------------------*/
ShmY::ShmY(double Ayini, double delyini, double omegyini,
           double totalty, double dty)
{
// CONSTRUCTOR ShmX: initializes Amplitud, ang. veloc., time, phase
   Ampy   = Ayini;
   delty  = delyini;
   omegy  = omegyini;
   timey  = totalty;
   deltty = dty;
   stepsy = (int)(totalty/deltty);
}
/*-------------------------------------*/
ShmY::~ShmY(void)
{
// DESTRUCTOR ShmY
   printf("Class ShmY destroyed\n");
}
/*--------------- ShmY Methods ----------------------------------------------*/
double ShmY::ypos(double partt)
{
// METHOD ShmY: returns Y=A sin(yt+d)
   return Ampy*sin(omegy*partt+delty);
}
/*-------------------------------------*/
void ShmY::archive()
{
// METHOD SgmY: Produces disk file with  X at several time intervals
   FILE  *pf;
   int i;
   double yy, tt;
   
   pf = fopen("shmy.dat","w");
   tt = 0.;
   for(i = 1 ; i <= stepsy ; i += 1)
   {
      yy=ypos(tt);
      fprintf(pf,"%f  %f\n",tt,yy);
      tt=tt+deltty;
   }
   fclose(pf);
}
/*-------------------------------------*/
/*---------------- ShmXY Class Definition -----------------------------------*/
class ShmXY : public ShmX, public ShmY
{
// CLASS ShmXY: child class of ShmX, ShmY; 2D simple harmonic motion
   public:  
      ShmXY(double Axini, double delxini, double omegxini, double totaltx,
            double dtx, double Ayini, double delyini, double omegyini,
            double totalty, double dty);
      ~ShmXY(void);
      void archive();
};
/*---------------- ShmXY Constructor and Destructor -------------------------*/
ShmXY::ShmXY(double Axini, double delxini, double omegxini, double totaltx,
             double dtx, double Ayini, double delyini, double omegyini,
             double totalty, double dty):
         ShmX(Axini, delxini, omegxini, totaltx, dtx),
         ShmY(Ayini, delyini, omegyini, totalty, dty)
{
// CONSTRUCTOR ShmXY: no initialization, all variables passed to ShmX, ShmY
}
/*-------------------------------------*/
ShmXY::~ShmXY(void)
{
// DESTRUCTOR ShmXY
   printf("Class ShmXY destroyed\n");
}
/*--------------- ShmXY Methods ---------------------------------------------*/
void ShmXY::archive()
{
// METHOD ShmXY: Produces disk file with  X at several time intervals
   FILE  *pf;
   int i;
   double xx, yy, tt;
   
   pf = fopen("shms.dat","w");
   tt = 0.;
   for(i = 1 ; i <= stepsx ; i += 1)
   {
      yy = ypos(tt);
      xx = xpos(tt);
      fprintf(pf,"%f  %f\n",yy,xx);                       /* y vs. x output  */
      tt = tt + delttx;
   }
   fclose(pf);
}
/*-------------------------------------*/
/*---------------- Main Program ---------------------------------------------*/
void main()
{
   double Ainix, delxx, wxx, txx, Ainiy, delyy, wyy, tyy, dx, dy;
   double pii;
   pii    = M_PI;
   Ainix  = 3.0;
   delxx  = 0.0;
   wxx    = 2.0;
   txx    = 7.0;
   tyy    = 7.0;
   Ainiy  = 2.0;
   delyy  = pii;
   wyy    = 3.0;
   dx     = 0.1;
   dy     = 0.1;
   ShmXY shmot(Ainix, delxx, wxx, txx, dx, Ainiy, delyy, wyy, tyy, dy);
   printf("  \n");
   printf("  \n");
   shmot.archive();
}
