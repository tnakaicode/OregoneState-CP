/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// rk45: Runge-Kutta-Fehlberg adaptive step size ODE solver

import java.io.*; 

public class rk45 {
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w =  new PrintWriter(new FileOutputStream("rk45.dat"), true);
    double h, t, s, s1, hmin, hmax, E, Eexact, error; 
    double y[]   = new double[2], fReturn[] = new double[2], ydumb[] = new double[2];
		double err[] = new double[2], k1[]      = new double[2], k2[]    = new double[2];
    double k3[]  = new double[2], k4[]      = new double[2], k5[]    = new double[2];
		double k6[]  = new double[2]; 
    double Tol = 1.0E-8, a = 0., b = 10;                // Error tolerance, Endpoints
    int i, j, flops, n = 20; 
		y[0] = 1. ;   y[1] = 0. ;                                           // Initialize
    h = (b-a)/n; hmin = h/64;   hmax = h*64;                // Min and max step sizes
    t = a;   j = 0; 
    long timeStart = System.nanoTime(); 
    System.out.println("" + timeStart + ""); 
    flops = 0;   Eexact = 0. ;  error = 0. ; 
    double sum = 0. ; 
		while (t < b)  {                                                // Loop over time
    	System.out.println("Rk45  t = " +t+" , x= "+y[0]+", v= "+y[1]);
      w.println(t+" " +y[0]);                                      // Output to file
      if ( (t + h) > b ) h = b - t;                                      // Last step
      f(t, y, fReturn);                     // Evaluate both RHS's, return in fReturn
      k1[0] = h*fReturn[0];     k1[1] = h*fReturn[1]; 
      for ( i=0; i <= 1; i++ ) ydumb[i] = y[i] + k1[i]/4; 
      f(t + h/4, ydumb, fReturn); 
      k2[0] = h*fReturn[0];     k2[1] = h*fReturn[1]; 
      for (i=0; i <= 1; i++) ydumb[i] = y[i]+3*k1[i]/32 + 9*k2[i]/32;
      f(t + 3*h/8, ydumb, fReturn); 
      k3[0] = h*fReturn[0];  
      k3[1] = h*fReturn[1]; 
      for ( i=0; i <= 1; i++ ) ydumb[i] = y[i] + 1932*k1[i]/2197
        -7200*k2[i]/2197. + 7296*k3[i]/2197; 
      f(t + 12*h/13, ydumb, fReturn); 
      k4[0] = h*fReturn[0];          k4[1] = h*fReturn[1];   
      for ( i=0; i <= 1; i++ ) ydumb[i] = y[i]+439*k1[i]/216 -8*k2[i]
        + 3680*k3[i]/513 -845*k4[i]/4104; 
      f(t + h, ydumb, fReturn); 
      k5[0] = h*fReturn[0];      k5[1] = h*fReturn[1];   
      for ( i=0; i <= 1; i++ ) ydumb[i] = y[i] -8*k1[i]/27 + 2*k2[i]
        -3544*k3[i]/2565 + 1859*k4[i]/4104 -11*k5[i]/40; 
      f(t + h/2, ydumb, fReturn); 
      k6[0] = h*fReturn[0];     k6[1] = h*fReturn[1]; 
      for ( i=0; i <= 1; i++ ) err[i] = Math.abs( k1[i]/360 
        - 128*k3[i]/4275 - 2197*k4[i]/75240 + k5[i]/50. +2*k6[i]/55);
			if ( err[0] < Tol || err[1] < Tol || h <= 2*hmin ) {        // Accept step size
        for ( i=0; i <= 1; i++ ) y[i] = y[i] + 25*k1[i]/216. 
          + 1408*k3[i]/2565. + 2197*k4[i]/4104. - k5[i]/5.;
        t = t + h; 
        j++ ; 
      }
      if ( err[0]==0 || err[1]==0 ) s = 0;                      // Trap division by 0
      else s = 0.84*Math.pow(Tol*h/err[0], 0.25);                      // Reduce step
	   if ( s  <  0.75 && h > 2*hmin )  h /= 2.;                       // Increase step
		 else if ( s > 1.5 && 2* h  <  hmax ) h *= 2.;      
     flops++ ; 
     E = Math.pow(y[0], 6.) + 0.5*y[1]*y[1]; 
     Eexact = 1. ; 
     error = Math.abs((E-Eexact)/Eexact);       
     sum += error;  
    }
    System.out.println(" < error> =  " + sum/flops); 
    System.out.println("flops = " + flops + ""); 
    long timeFinal = System.nanoTime(); 
    System.out.println("" + timeFinal + ""); 
    System.out.println("Nanoseconds = " + (timeFinal-timeStart)); 
  }  
                                                            // Enter your own RHS here!
  public static void f(double t, double y[], double fReturn[])  {
    fReturn[0] = y[1]; 
    fReturn[1] = -6.*Math.pow(y[0], 5.);  
    return; 
  }
} 