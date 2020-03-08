/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
   */
// rk4.java: 4th order Runge-Kutta ODE Solver for arbitrary y(t)
import java.io.*; 

public class rk4  {
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w = new PrintWriter(new FileOutputStream("rk4.dat"), true);
    double h, t, a = 0., b = 10.;                       // Step size, time, endpoints
    double ydumb[] = new double[2], y[]  = new double[2], fReturn[] = new double[2];
    double k1[]    = new double[2], k2[] = new double[2], k3[]      = new double[2];
    double k4[]    = new double[2];    
    int i, n = 100; 
    y[0] = 3. ;    y[1] = -5. ;                                         // Initialize
    h = (b-a)/n;  
    t = a; 
    w.println(t + " " + y[0] + " " + y[1]);                            // File output
    while (t < b)  {                                                     // Time loop
      if ( (t + h) > b ) h = b - t;                                      // Last step
      f(t, y, fReturn);                          // Evaluate RHS's, return in fReturn
      k1[0] = h*fReturn[0];  k1[1] = h*fReturn[1];         // Compute function values
      for ( i=0; i <= 1; i++ )  ydumb[i] = y[i] + k1[i]/2; 
      f(t + h/2, ydumb, fReturn); 
      k2[0] = h*fReturn[0];  k2[1] = h*fReturn[1]; 
      for ( i=0; i <= 1; i++ )  ydumb[i] = y[i] + k2[i]/2; 
      f(t + h/2, ydumb, fReturn); 
      k3[0] = h*fReturn[0];  k3[1] = h*fReturn[1]; 
      for ( i=0; i <= 1; i++ )  ydumb[i] = y[i] + k3[i]; 
      f(t + h, ydumb, fReturn); 
      k4[0] = h*fReturn[0];   k4[1] = h*fReturn[1];   
      for (i=0;i <= 1; i++)y[i]=y[i]+(k1[i]+2*(k2[i]+k3[i])+k4[i])/6;
      t = t + h;    
      w.println(t + " " + y[0] + " " + y[1]);                          // File output
    }                                                               // End while loop
    System.out.println("Output in rk4.dat");
  } 
                                                                // YOUR FUNCTION here
  public static void f( double t, double y[], double fReturn[] )  
    { fReturn[0] = y[1];                                                // RHS 1st eq
      fReturn[1] = -100*y[0]-2*y[1] + 10*Math.sin(3*t); }                  // RHS 2nd
} 