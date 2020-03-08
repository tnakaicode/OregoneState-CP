/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
/* SplineAppl.java: Application version of cubic spline fitting. Interpolates 
   array x[n], y[n], x0 < x1 ...  < x(n-1). yp1, ypn: y' at ends evaluated internally
   y2[]: y" array; yp1, ypn > e30 for natural spline   */
import java.io.*; 

public class SplineAppl  {
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w = new PrintWriter(new FileOutputStream("Spline.dat"), true);
    PrintWriter q = new PrintWriter(new FileOutputStream("Input.dat"), true);
    double x[] = {0.,1.2,2.5,3.7,5.,6.2,7.5,8.7,9.9};                        // input
    double y[] = {0.,0.93,.6,-0.53,-0.96,-0.08,0.94,0.66,-0.46};
    int  i,n = x.length,  np = 15, klo, khi, k; 
    double y2[] = new double[9], u[] = new double[n]; 
    double h, b, a, Nfit, p, qn, sig, un, yp1, ypn, xout, yout; 
    for ( i=0; i < n; i++ ) q.println (" " + x[i] + " " + y[i] + " ");
    Nfit = 30;                                                        // N output pts
    yp1 = (y[1]-y[0])/(x[1]-x[0]) - (y[2]-y[1])/(x[2]-x[1]) +(y[2]-y[0])/(x[2]-x[0]); 
    ypn = (y[n-1]-y[n-2])/(x[n-1]-x[n-2]) - (y[n-2]-y[n-3])
                     /(x[n-2]-x[n-3]) + (y[n-1]-y[n-3])/(x[n-1]-x[n-3]);
    if (yp1 > 0.99e30) y2[0] = u[0] = 0. ; // Natural
		  else {y2[0] = (-0.5); u[0] = (3/(x[1]-x[0]))*((y[1]-y[0])/(x[1]-x[0])-yp1);}
    for ( i=1;  i <= n-2;  i++ )  {                             // Decomposition loop
      sig = (x[i]-x[i-1])/(x[i + 1]-x[i-1]); 
      p = sig*y2[i-1] + 2. ; 
      y2[i] = (sig-1.)/p; 
      u[i] = (y[i+1]-y[i])/(x[i + 1]-x[i])-(y[i]-y[i-1])/(x[i]-x[i-1]); 
      u[i] = (6.*u[i]/(x[i+1]-x[i-1])-sig*u[i-1])/p;  
    }
    if (ypn > 0.99e30) qn = un = 0. ;                            // Test for natural
		 else {qn = 0.5; un = (3/(x[n-1]-x[n-2]))*(ypn-(y[n-1]-y[n-2])/(x[n-1]-x[n-2]));}
    y2[n-1] = (un-qn*u[n-2])/(qn*y2[n-2] + 1.); 
    for ( k = n-2;  k>= 0; k--)  y2[k] = y2[k]*y2[k + 1] + u[k];  
    for ( i=1;  i <= Nfit;  i++ ) {                 // initialization ends, begin fit                                 
      xout = x[0] + (x[n-1]-x[0])*(i-1)/(Nfit); 
      klo = 0;                                                     // Bisection algor
      khi = n-1;                                       // klo, khi bracket xout value
      while (khi-klo >1) {k = (khi+klo) >> 1; if (x[k] > xout) khi =k; else klo = k;}
      h = x[khi]-x[klo]; 
      if (x[k] > xout) khi = k;   else klo = k; 
      h = x[khi]-x[klo];  a = (x[khi]-xout)/h; 
      b = (xout-x[klo])/h; 
      yout = (a*y[klo] + b*y[khi] +((a*a*a-a)*y2[klo] + (b*b*b-b)*y2[khi])*(h*h)/6.);
      w.println (" " + xout + " " + yout + " ");
    }
    System.out.println("data stored in Spline.dat");  
} } 