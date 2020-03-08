/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Daub4Compress2.java: Wavelet compression with Daub4 wavelets
import java.io.*; 
import java.util.*; 
  
public class Daub4Compress2  {
  static double c0, c1, c2, c3;                             // Global Daub4 variables
  static int n;                                                    // 2^n data points
  
  public static void pyram(double f[], int n, int sign) {
 // 1D DWT via pyramid algorithm replaces x[] by TF, (sign = +/-1 for TF/inverse
    int nd, nend; 
    double sq3, fsq2;                                            // sqrt(3), 4sqrt(2)
    if (n  <  4) return;                                              // Too few data
    sq3 = Math.sqrt(3);     
    fsq2 = 4.*Math.sqrt(2); 
    c0 = (1. + sq3)/fsq2;                                 // Daubechies 4 coefficents
    c1 = (3. + sq3)/fsq2; 
    c2 = (3.-sq3)/fsq2; 
    c3 = (1.-sq3)/fsq2; 
    nend = 4;                                 // Controls output (1:1024, 2:512, etc)
    if (sign >= 0) {for ( nd=n; nd>=nend; nd/=2) daube4(f, nd, sign);}
    else {for ( nd=4; nd <= n; nd*=2) daube4(f, nd, sign); }
  }                                                                          // Pyram
  
  public static void daube4(double f[], int n, int sign){
    double tr[] = new double [n + 1];                                // Temp variable
    int i, j; 
    int mp, mp1; 
    double tol = (40./100.)*868. ;                             // Arbitrary threshold
     // Donoho's universal thresholding for earthquake data 
     // tol = var*sqrt(2*log(n)), var = estimate of noise variance
     // deselect the following line when process earthquake data
     // double tol = 1.1*0.013481721*Math.sqrt(2*Math.log(n));    
    if (n  <  4) return; 
    mp = n/2;                                                    // Midpoint of array
    mp1 = mp + 1;                                                // Midpoint plus one
    if (sign >= 0) {            // DWT
      j = 1; 
      for ( i=1; j <= n-3; i++ ) {
        tr[i] = c0*f[j] + c1*f[j + 1] + c2*f[j + 2] + c3*f[j + 3]; 
        tr[i + mp] = c3*f[j]-c2*f[j + 1] + c1*f[j + 2]-c0*f[j + 3];  
        if (Math.abs(tr[i + mp])  <  tol)  tr[i + mp] = 0. ;               // d coeff
        j += 2; 
      }
      tr[i] = c0*f[n-1] + c1*f[n] + c2*f[1] + c3*f[2];                    // Low-pass
      tr[i + mp] = c3*f[n-1]-c2*f[n] + c1*f[1]-c0*f[2];                  // High-pass
      if (Math.abs(tr[i + mp])  <  tol) tr[i + mp] = 0. ; 
      }  
			else {                                                          // Inverse DWT
      tr[2] = c3*f[mp]-c0*f[n] + c1*f[1]-c2*f[mp1];               // High-pass filter
      for ( i=1, j=3; i < mp; i++ )  {
        tr[j] = c2*f[i] + c1*f[i + mp] + c0*f[i + 1] + c3*f[i + mp1];
        j += 1;                                          // Upsampling c coefficients
        tr[j] = c3*f[i]-c0*f[i + mp] + c1*f[i + 1]-c2*f[i + mp1];
        j += 1;                                          // Upsampling d coefficients
      }                                                                        // For
    }                                                                         // Else
    for ( i=1; i <= n; i++ )   f[i] = tr[i];                        // Copy transform
  }

  public static void main(String[] argv) throws IOException, FileNotFoundException {
     // initial data in indata.dat, compressed image in outdata
    PrintWriter w = new PrintWriter (new FileOutputStream("indata.dat"), true); 
    PrintWriter q = new PrintWriter (new FileOutputStream("outdata.dat"), true);   
    int n = 1024, i;          // For arbitrary function, n = 8192 for earthquake data
    double x[] = new double[n + 1];                                    // Data vector
    for ( i=1; i <= n; i++ )  {                // Initial data via arbitrary function
      x[i] = (1./1204)* (i*i-3.*i);  
      w.printf("%d\t%8.3f\n", i, x[i]); 
    }
     // deselect the following lines to process earthquake data
   /*  Scanner in = new Scanner(new File("earthquake.dat")); 
    i = 1; 
    while (in.hasNextDouble()) {a[i] = in.nextDouble(); 
			w.printf("%d\t%8.3f\n", i, a[i]); i++ ; 
    }     */
   pyram(x, n, 1);                                                             // DWT
   pyram(x, n, -1);                                                    // Inverse DWT
   for ( i=1; i <= n; i++ ) q.printf("%d\t%8.3f\n", i, x[i]); 
  }                                                                           // Main
}                                                                            // Class