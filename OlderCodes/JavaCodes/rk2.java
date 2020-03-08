/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   rk2.java:  Runge-Kutta 2nd order  ODE solver                         */
import java.io.*; 

public class rk2  {
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {										
    PrintWriter w = new PrintWriter(new FileOutputStream("rk2.dat"), true);					
    double h, t, a = 0., b = 10. ;                                       // Endpoints
    double y[]  = new double[2], ydumb[] = new double[2], fReturn[] = new double[2];  
    double k1[] = new double[2], k2[]    = new double[2];  
    int i, n=100; 
    y[0] = 3. ;   y[1] = -5. ;                                          // Initialize
    h = (b-a)/n;  
    t = a; 
    System.out.println("rk2  t="+t+" , x = " +y[0]+ ", v = " + y[1]);  
    w.println(t + " " + y[0] + " " + y[1]);                         // Output to file                                         
    while (t < b)  {                                                // Loop over time
      if ( (t + h) > b ) h = b - t;                                  // The last step
      f(t, y, fReturn);                          // Evaluate RHS's and return fReturn
      k1[0] = h*fReturn[0];  k1[1] = h*fReturn[1];         // Compute function values
      for ( i=0; i <= 1; i++ ) ydumb[i] = y[i] + k1[i]/2; 
      f(t + h/2, ydumb, fReturn); 
      k2[0] = h*fReturn[0];  k2[1] = h*fReturn[1]; 
      for ( i=0; i <= 1; i++ ) y[i] = y[i] + k2[i]; 
      t = t + h; 
      System.out.println("rk2  t=" +t+ " , x = "+y[0]+", v = "+y[1]); 
      w.println(t + " " + y[0] + " " + y[1]);                       // Output to file
    }                                                               // End while loop
  }     
                                                  // RHS FUNCTION of your choice here
  public static void f(double t, double y[], double fReturn[])     
    { fReturn[0] = y[1];                                     // RHS of first equation
      fReturn[1] = -100*y[0]-2*y[1] + 10*Math.sin(3*t); } 
} 