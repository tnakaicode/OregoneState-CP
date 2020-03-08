/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  Shock.java: Formation of Shockwave from Burger's eqnt
import java.io.*;

public class Shock  {
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w = new PrintWriter (new FileOutputStream("Shock.dat"), true);
    PrintWriter q = new PrintWriter(new FileOutputStream("initial.dat"), true);
    int m = 100, i, j, n;                                    // Number of grid points
    double u[] = new double[m+1], u0[] = new double[m+1];      // Final, initial wave
    double epsilon = 1.0, beta = 0.1, x, dx, dt, T_final;   // Wave speed, CFL number
    dx = 2.0/m;  dt = beta*dx/epsilon;                            // Space, time step
    T_final = 0.15;
    n = (int)(T_final/dt);System.out.println(""+n+"");
    for ( i=0; i < m-1; i++ )  {                                       // Initial data
       x = i*dx;
       u0[i] = 3.0*Math.sin(3.2*x);
       q.println(""+0.01*i+" "+u0[i]+" ");  
    }
    for ( j=1;j < n;j++)  {                                    // Lax-Wendroff scheme
      for ( i=0;i < m-1;i++ )  {
        u[i+1] = u0[i+1] - (u0[i+2]*u0[i+2] - u0[i]*u0[i])*(0.25*beta)+
         + (((u0[i+2]+u0[i+1])/2.0)*(u0[i+2]*u0[i+2] - u0[i+1]*u0[i+1])-
         ((u0[i+1]+u0[i])/2.0)*(u0[i+1]*u0[i+1]-u0[i]*u0[i]))*0.25*beta*beta;
        u[0]=0.0;      u[m-1]=0.0;
        if ( j%5==0 && i%4==0 )  w.println(" "+u[i]);
        u0[i] = u[i];                                             // Shift new to old
      }
      if ( j%5 == 0) w.println("");
    }
    System.out.println("Numerical solution data saved in Shock.dat");
  }                                                                           // Main
}                                                                            // Class