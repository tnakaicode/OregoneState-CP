/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
//Planet position with the velocity version of the Verlet algorithm.

import java.io.*;
public class Verlet
	{

public static void main(String[] argv)throws IOException,
FileNotFoundException

{PrintWriter w = 
        new PrintWriter(new FileOutputStream("Verlet.dat"), true);
/* save data in Verlet.dat */ 
int i;
int n =41001; /*number of iterations*/
int n1  = 200; /*save data in Verlet.dat every n1 iterations*/
double h; /* step */
double x[]= new double[n];
double y[]= new double[n];
double vx[]= new double[n];
double vy[]= new double[n];
double ax[]= new double[n];
double ay[]= new double[n];
double t[]= new double[n];
double r[]= new double[n];
/* initialization*/
t[0] = 0;
x[0] = 0.5;
y[0] = 0;
vx[0] = 0;
vy[0] = 1.63;
VerletV(t,x,y,vx,vy,ax,ay,r,n);
 /* write data gnuplot 2D format */
for (i = 0; i < n; i++)
  {if((i%n1) == 0)
  w.println(" "+y[i]+" "+x[i]+" ");
  }
System.out.println("data stored in Verlet.dat");
}
/* Verlet algorithm for the position and velocity */
public static void VerletV(double t[],double x[],double y[],
double vx[],double vy[],double ax[],double ay[],double r[],int n)
{
 double h = 1.0/10000;int i;double r3;
r[0] = x[0];
ax[0] = -x[0]/Math.pow(r[0],3);
ay[0] = -y[0]/Math.pow(r[0],3);
for (i = 0; i < n-1; ++i)
  {
  t[i+1]  = h*(i+1);
  x[i+1]  = x[i]+h*vx[i]+(h*h/2)*ax[i];
  y[i+1]  = y[i]+h*vy[i]+(h*h/2)*ay[i];
  r[i+1] = Math.sqrt(x[i+1]*x[i+1]+y[i+1]*y[i+1]);
  r3 = (x[i+1]*x[i+1]+y[i+1]*y[i+1])*r[i+1];
  ax[i+1] = -x[i+1]/r3;
  ay[i+1] = -y[i+1]/r3;
  vx[i+1]= vx[i]+h*(ax[i+1]+ax[i])/2;
  vy[i+1]= vy[i]+h*(ay[i+1]+ay[i])/2;
  }
}


}