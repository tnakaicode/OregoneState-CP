/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
import java.io.*;
  //Catenary with friction by Juan Vanegas
public class CatFriction      
  
{ 
  public static void main(String[] argv)
  throws IOException, FileNotFoundException 
{

double rho, T, maxtime,kappa,D,dx,dt;
dt = .0001;
dx=.01;
T = 1;
rho = .1;
D = T/(rho*9.8);
maxtime = 1000;
kappa = 30;
int i, k;
double x[][] = new double[101][3];
PrintWriter q = new PrintWriter(new FileOutputStream("catfrict.dat"),true);
PrintWriter r = new PrintWriter(new FileOutputStream("funct.dat"),true);

//for (i=0; i<81; i++) {
//  x[i][0] = .00125 * i;
//}
//
//for (i=81; i<=100; i++) {
//  x[i][0] = 0.1 - .005*(i-80);
//}

for(i=0; i<101; i++)      /* initial configuration */
      {
    x[i][0] = -0.08*Math.sin(Math.PI*i*dx); 
      }

//first step
for (i=1; i<100; i++) {
  x[i][1] = (  dt*(T/rho)*(  ( x[i+1][0]-x[i][0] )/dx*( Math.exp((i-50)*dx/D)-Math.exp(-(i-50)*dx/D) )/D + ( Math.exp((i-50)*dx/D)+Math.exp(-(i-50)*dx/D) ) * ( x[i+1][0]+x[i-1][0]-2.0*x[i][0] )/(Math.pow(dx,2))  )-2*kappa*x[i][0]+2*x[i][0]/dt  )/(2*kappa+(2/dt));
  
  }
  
//rest of steps
for (k=1; k<maxtime; k++) {
  for (i=1; i<100; i++) {
    x[i][2] = (dt*(T/rho)*((x[i+1][1]-x[i][1])/dx*(Math.exp((i-50)*dx/D)-Math.exp(-(i-50)*dx/D))/D +  (Math.exp((i-50)*dx/D)+Math.exp(-(i-50)*dx/D)) * (x[i+1][1]+x[i-1][1]-2.0*x[i][1])/(Math.pow(dx,2)))-2*kappa*x[i][1]-(-2*x[i][1]+x[i][0])/dt)/(2*kappa+(1/dt));
  
  }
  
  for (i=1; i<101; i++) {
    x[i][0] = x[i][1];
    x[i][1] = x[i][2];
  }
  
  if ((k%5) == 0) {
    for (i=0;i<100;i++) {
    q.println(+x[i][2]);
    r.println(+(D*(Math.exp((i-50)*dx/D)+Math.exp(-(i-50)*dx/D))));
  }
  q.println(" ");
  r.println(" ");
  }
  }
  System.out.println("data stored in catfrict.dat and funct.dat");
  }     


}
  
  