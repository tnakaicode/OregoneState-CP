/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Numerov.java solves Schroedinger eq via Numerov method (rk also OK)

public class Numerov  {
  int steps = 1000;                                   // Half width of potential well
  static double V = -0.001, eps = 1E-5, Emin = -0.001, Emax = -0.00085;    
  public static void main(String[] argv) {
    double E, min, max; 
    int i = 0;                                              // Counter for iterations
    min = Emin;  max = Emax;                 
    do  { 
      i++ ;   
      E = (max + min)/2. ;                                     // Divide energy range
      System.out.println("eigenvalue E = "+E+", diff(E) = " +diff(E)); 
      System.out.println("after " + i + " iterations"); 
      if ( diff(max)*diff(E)>0 ) max = E;  else min = E;                 // Bisection
    } 
    while(Math.abs(diff(E))>eps); 
    System.out.println("eigenvalue E = " + E); 
    System.out.println("after " + i + " iterations");   
  }
  
  public static double diff(double E)  {
    double ud[] = new double[1501], u[] = new double[1501], k2l[] = new double[1501];
		double k2r[] = new double[1501], xl0, xr0, xl, xr, plus = 0., minus = 0., h; 
    int i, im, nl, nr, n = 1501; 
    u[0] = 0. ;   u[1] = 0.00001; 
    xl0 = -1000;   xr0 = 1000; 
    h  = (xr0-xl0)/(n-1); 
    for ( i = 0;  i  <  n; ++ i)  { 
      xl = xl0 + i*h; 
      xr = xr0-i*h; 
      k2l[i] = (E-v(xl)); 
      k2r[i] = (E-v(xr));     
    }
    im = 500;                                                   // The matching point
    nl = im + 1; 
    nr = n-im + 1; 
    for ( i=1; i < nl-1; i++ )  { u[i + 1] = (2*u[i]*(1-5.*h*h/12.*k2l[i])
				                  - (1.+ h*h/12.*k2l[i-1])*u[i-1])/(1.+ h*h/12.*k2l[i+1]); }
    minus = (u[i + 1]-u[i])/h*u[i];                         // Logarithmic derivative
    u[0] = 0. ; u[1] = 0.00001; 
    for ( i=1; i < nr-1; i++ )  { u[i+1] = (2*u[i]*(1-5.*h*h/12.*k2r[i])-
                 (1.+ h*h/12.*k2r[i-1])*u[i-1])/(1.+ h*h/12.*k2r[i+1]); }
    plus = (u[i + 1]-u[i])/h*u[i];                          // Logarithmic derivative
    System.out.println ("minus-plus/minus + plus = " + ((minus-plus)/(minus+plus)));
    return((minus-plus)/(minus + plus)); 
  }
  
  public static double v(double x)  {   
    double v;       
    if (Math.abs(x) <= 500)  v = -0.001;   else v = 0; 
    return v; 
  }
}
