/* 
************************************************************************
*  soliton.java: Solves the Kortewg-deVries Equation using a finite    *
*             difference method                                        *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*                                                                      *
*  comment: Output data is saved in 3D grid format used by gnuplot     *
************************************************************************
*/
import java.io.*;
public class solitoncross1
{
static double ds =0.4;        /* delta x */
static double dt =0.1;        /* delta t */
static int max= 10000;      /* time steps */
static double mu =0.1;        /* mu from KdeV equation */
static double eps =0.2;       /* epsilon from KdeV eq. */

public static void main(String[] argv) throws IOException,
FileNotFoundException
{
   int i, j, k;
   double a1,a2,a3, fac, time;
    double u[][]=new double[131][3];
  
        /* save data in soliton.dat */
  
   PrintWriter w =
        new PrintWriter(new FileOutputStream("soliton.dat"), true);
   for(i=0; i<131; i++)     /* initial wave form */
   {
     u[i][0] = 0.8*(1.0 - Math.pow((Math.exp(2*(0.25*ds*i - 3.0))-1)/(Math.exp(2*(0.25*ds*i - 3.0))+1),2))+
     0.3*(1.0 - Math.pow((Math.exp(2*((4.5/26.0)*ds*i - 4.5))-1)/(Math.exp(2*((4.5/26.0)*ds*i - 4.5))+1),2));
   //u[i][0] = 0.8*(1.0 - ((Math.exp(2*(0.25*ds*i - 3.0))-1)/(Math.exp(2*(0.25*ds*i - 3.0))+1)))+
     // 0.3*(1.0 - ((Math.exp(2*((4.5/26.0)*ds*i - 4.5))-1)/(Math.exp(2*((4.5/26.0)*ds*i - 4.5))+1)));
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
      u[i][1] = u[i][0] - 0.029*a1*a3 - 0.029*fac*a2/3.;      
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
         u[i][2] = u[i][0] - 0.029*a1*a3 - 0.029*2.*fac*a2/3.;    
      }

      for(k=0; k<131; k++)    /* move one step ahead */
      {
         u[k][0] = u[k][1];
         u[k][1] = u[k][2];
      }
     
      if((j%350)==0)      /* plot every 200th step */
      {
         for(k=0; k<131; k+=2)
         {
            w.println(""+ u[k][2]+"");   /* gnuplot 3D format */
         }
         w.println( "");         /* empty line for gnuplot */
      }
   }
   System.out.println("data stored in soliton.dat");
  
}

}
