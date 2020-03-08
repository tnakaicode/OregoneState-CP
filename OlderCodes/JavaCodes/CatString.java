/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
//CatString.java by Juan M. Vanegas

import java.io.*;

public class CatString
{
    public static void main(String[] argv)throws IOException, FileNotFoundException
    {
  int i,k, N=100;
  double u[][] = new double[N+1][3];
  double rho, To, dx, dt;
  rho = 0.01;
  To = 0.5;
  dx = 0.01;
  dt = 0.0001;
  PrintWriter w = new PrintWriter(new FileOutputStream("disturb.dat"), true);
  PrintWriter q = new PrintWriter(new FileOutputStream("function.dat"), true);
  for(i=0; i<51; i++)     /* initial configuration */
      {
    u[i][0] = 0.00088*i; 
      }
  for(i=51; i<101; i++)
      {
    u[i][0] = 0.044-0.00088*(i-50);  
      }
  for(i=1; i<100; i++)            /* first time step  */
      {
    u[i][1] = u[i][0] + (4.9*dt*dt/(2*dx))*(Math.exp((i-50)*dx*9.8*rho/To)-Math.exp(-(i-50)*dx*9.8*rho/To))*(u[i+1][0]-u[i][0]) + (To*dt*dt/(4*rho*dx*dx))*(Math.exp((i-50)*dx*9.8*rho/To)+Math.exp(-(i-50)*dx*9.8*rho/To))*(u[i+1][0]+u[i-1][0]-2*u[i][0]);        
      }
  
  for(k=1; k<10001; k++)                 /* all later time steps  */
      {
    for(i=1; i<100; i++)
        {
      u[i][2] = 2*u[i][1] - u[i][0] + (4.9*dt*dt/dx)*(Math.exp((i-50)*dx*9.8*rho/To)-Math.exp(-(i-50)*dx*9.8*rho/To))*(u[i+1][1]-u[i][1]) + (To*dt*dt/(2*rho*dx*dx))*(Math.exp((i-50)*dx*9.8*rho/To)+Math.exp(-(i-50)*dx*9.8*rho/To))*(u[i+1][1]+u[i-1][1]-2*u[i][1]);        
        }
    for(i=0; i<101; i++)
        { 
      u[i][0] = u[i][1];    
      u[i][1] = u[i][2];     
        }
    
    if((k%40) == 0)                     /* print every 5th point */
        {
      for(i=0; i<101; i++) 
          { 
        w.println(" " +u[i][2]+ " "); /* gnuplot 3D grid format */
        q.println(" "+(To/(19.6*rho)*(Math.exp((i-50)*dx*9.8*rho/To)+Math.exp(-(i-50)*dx*9.8*rho/To))+u[i][2])+" ");
          }           /* empty line for gnuplot */
      w.println(" ");
      q.println(" ");
        }  
      }
    }
}









