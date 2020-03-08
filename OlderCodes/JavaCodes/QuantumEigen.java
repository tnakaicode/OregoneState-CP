/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// QuantumEigen.java: solves Schroedinger eq via rk4 + Bisection Algor
import java.io.*; 

public class QuantumEigen  {
  static double eps = 1E-6 ;                           // Class variables;  precision
  static int n_steps = 501;                                       // Number int steps
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    double E = -17., h = 0.04;                   // Initial E in MeV, step size in fm
    double Emax, Emin, Diff;  
    int count, count_max = 100; 
    Emax = 1.1*E;     Emin = E/1.1; 
    for ( count=0;  count <= count_max;  count++ ) {                // Iteration loop
      E = (Emax + Emin)/2. ;                                        // Divide E range
      Diff = diff(E, h); 
      System.out.println("E = " + E + ", L-R Log deriv(E) = " + Diff); 
      if (diff(Emax, h)*Diff > 0) Emax = E;                    // Bisection algorithm
      else Emin = E;     
      if ( Math.abs(Diff)  <  eps ) break; 
    }
    plot(E, h); 
    System.out.println("Final eigenvalue E = " + E); 
    System.out.println("iterations, max = " + count + ", " + count_max);       
    System.out.println("WF in QunatumL/R.dat, V in QuantumV.dat "); 
  }                                                                       // End main
                                                             // Returns L-R log deriv
  public static double diff(double E, double h) 
                                          throws IOException, FileNotFoundException {
    double left, right, x; 
    int ix, nL, nR, i_match; 
    double y[] = new double[2];  
    i_match = n_steps/3;                                           // Matching radius
    nL = i_match + 1;  
    y[0] = 1.E-15;                                              // Initial wf on left
    y[1] = y[0]*Math.sqrt(-E*0.4829);    
    for (ix = 0; ix < nL + 1; ix++) { x = h * (ix  -n_steps/2); rk4(x, y, h, 2, E); }
    left = y[1]/y[0];                                              // Log  derivative
    y[0] = 1.E-15;                              // - slope for even;  reverse for odd
    y[1] = -y[0]*Math.sqrt(-E*0.4829);                             // Initialize R wf
    for (ix = n_steps; ix > nL+1; ix--){x = h*(ix+1-n_steps/2); rk4(x, y, -h, 2, E);}
    right = y[1]/y[0];                                              // Log derivative
    return( (left - right)/(left + right) ); 
  }
  
  public static void plot(double E, double h)         // Repeat integrations for plot
                                         throws IOException, FileNotFoundException {
    PrintWriter L =  new PrintWriter(new FileOutputStream("QuantumL.dat"), true);
    PrintWriter R =  new PrintWriter(new FileOutputStream("QuantumR.dat"), true);
    PrintWriter Vx = new PrintWriter(new FileOutputStream("QuantumV.dat"), true);
    double left, right, normL, x = 0.; 
    int ix, nL, nR, i_match, n_steps = 1501;           // Total no integration steps
    double y[] = new double[2], yL[][] = new double [2][505]; 
    i_match = 500;                                                  // Matching point
    nL = i_match + 1;  
    y[0] = 1.E-40;                                          // Initial wf on the left
    y[1] = -Math.sqrt(-E*0.4829) *y[0];    
    for ( ix = 0;  ix <= nL;  ix++ ) { 
      yL[0][ix] = y[0];  yL[1][ix] = y[1]; 
      x = h * (ix  -n_steps/2); 
      rk4(x, y, h, 2, E); 
    }                                                        // Integrate to the left
    y[0] = -1.E-15;                                // - slope: even;  reverse for odd
    y[1] = -Math.sqrt(-E*0.4829)*y[0]; 
    for ( ix = n_steps -1;   ix >= nL + 1;  ix--)  {                  // Integrate in
      x = h * (ix + 1 -n_steps/2); 
      R.println(x + "  " + y[0] + "  " + y[1]);                         // File print
      Vx.println(x + "  " + 1.7E9*V(x));                                  // Scaled V
      rk4(x, y, -h, 2, E);        
    }
    x = x  - h; 
    R.println(x + "  " + y[0] + "  " + y[1]);                           // File print
    normL = y[0]/yL[0][nL];                          // Renormalize L wf & derivative
    for ( ix = 0;  ix <= nL;  ix++ )  { 
      x = h * (ix-n_steps/2 + 1); 
      y[0] = yL[0][ix]*normL; 
      y[1] = yL[1][ix]*normL; 
      L.println(x + "  " + y[0] + "  " + y[1]);                         // File print
      Vx.println(x + "  " + 1.7E9*V(x));                                   // Print V
    }                             
      return;              
  }
      
   public static void f(double x, double y[], double F[], double E)
     { F[0] = y[1];    F[1] = -(0.4829)*(E-V(x))*y[0];  }
   
  public static double V(double x)   
    { if (Math.abs(x)  <  10.)  return ( -16.);   else return(0.) ; }
    
  public static void rk4(double t, double y[],double h,int Neqs,double E){
    int i;
    double F[]  = new double[Neqs], ydumb[]     = new double[Neqs];
    double k1[] = new double[Neqs]; double k2[] = new double[Neqs]; 
    double k3[] = new double[Neqs]; double k4[] = new double[Neqs];  
    f(t, y, F,E);
    for (i=0; i<Neqs; i++) { k1[i] = h*F[i]; ydumb[i] = y[i] + k1[i]/2;}
    f(t + h/2, ydumb, F,E);
    for (i=0; i<Neqs; i++)  { k2[i] = h*F[i]; ydumb[i] = y[i] + k2[i]/2;}
    f(t + h/2, ydumb, F,E);
    for (i=0; i<Neqs; i++)  { k3[i]=  h*F[i]; ydumb[i] = y[i] + k3[i];}
    f(t + h, ydumb, F,E);
    for (i=0; i<Neqs; i++) {k4[i]=h*F[i]; y[i]=y[i]+(k1[i]+2*(k2[i]+k3[i])+k4[i])/6;}
  }
} 