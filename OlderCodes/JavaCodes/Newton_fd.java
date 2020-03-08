/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Newton_fd.java: Newton-Raphson root finder with forward diff deriv
public class Newton_fd  { 
  
  public static double f(double x) {return 2*Math.cos(x)-x; }             // Function
    
  public static void main(String[] argv) {   
    double eps = 1e-6, df, x = 2., dx = 1e-2;                            // Tolerance
    int it, imax = 100;                                       // Max no of iterations
    for ( it=1;  it <= imax;  it++ )  {
      df = ( f(x + dx) - f(x))/dx;                   // Forward difference derivative
      dx = -f(x)/df; 
      x += dx;                                                           // New guess
      System.out.println("Iterate # = "+it+"  x = "+x+" f(x)= "+f(x)); 
      if ( Math.abs(f(x)) <= eps )  {                        // Check for convergence
        System.out.println("Root found with precison = " + eps); 
        break; 
}  }  }  }
