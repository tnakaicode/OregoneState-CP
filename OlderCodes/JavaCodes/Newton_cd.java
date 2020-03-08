/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Newton_cd.java: Newton-Raphson root finder, central diff derivative

public class Newton_cd  {

  public static double f(double x) { return 2*Math.cos(x) - x; }          // function
   
  public static void main(String[] argv) {   
    double x = 2., dx = 1e-2, F= f(x), eps = 1e-6, df;   
    int it, imax = 100;                             // Max no of iterations permitted
    for ( it = 0;  it <= imax;  it++ )  {                                
      System.out.println("Iteration # = "+it+" x = "+x+" f(x) = "+F);
      df = ( f(x + dx/2) - f(x-dx/2) )/dx;                      // Central diff deriv
      dx = -F/df; 
      x += dx;                                                           // New guess
      F = f(x);                                                       // Save for use
      if ( Math.abs(F) <= eps )  {                           // Check for convergence
         System.out.println("Root found, tolerance eps = " + eps); 
         break; 
} } } }
