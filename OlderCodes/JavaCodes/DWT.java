/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// DWT.java: DUAB4 Wavelet TF; input=indata.dat, output=outdata.dat
// sign = + 1: DWT, -1: InvDWT,  2**n input  
import java.io.*; 

public class DWT {
  static double c0, c1, c2, c3;                                   // Global variables
  static int N = 1024, n;                                         // 2^n  data points
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w = new PrintWriter (new FileOutputStream("indata.dat"), true);
    PrintWriter q = new PrintWriter (new FileOutputStream("outdata.dat"), true);
    System.out.println("DWT: input = indata.dat, output = outdata.dat");
    int i, dn;  
    double xi, inxi;  double f[] = new double[N + 1];                  // Data vector
    inxi = 1./(double)(N);                                   // For chirp 0 <= t <= 1
    xi = 0. ; 
    for ( i=1; i <= N; i++ ) {
      f[i] = chirp(xi);                                    // Change for other signal
      xi = xi + inxi;   
      w.println(" " + xi + " " + f[i] + " ");                           // Indata.dat
    }
    n = N;                                     // number datapoints must = power of 2
    pyram(f, n, 1);                                     // DWTF (1 -> -1 for inverse)
    // pyram(f, n, -1);                                      // (1 -> -1 for inverse)
    for (i=1; i <= N; i++ ) q.println(" " +i+ " " +f[i]+ " ");  
  }                                                                           // Main
  
  public static void pyram(double f[], int n, int sign) {                  // Pyramid
    int nd, nend; 
    double sq3, fsq2;                                            // sqrt(3), 4sqrt(2)
    if (n  <  4) return;                                              // Too few data
    sq3 = Math.sqrt(3); 
    fsq2 = 4.*Math.sqrt(2);                                     // DAUB4 coefficients
    c0 = (1. +sq3)/fsq2; c1 = (3. +sq3)/fsq2; c2 = (3.-sq3)/fsq2; c3 = (1.-sq3)/fsq2;
		nend = 4;                                 // Controls output (1:1024, 2:512, etc)
    if (sign >= 0) for (nd=n;  nd >= nend;  nd /= 2) daube4(f, nd, sign); 
       else for (nd=4;  nd <= n;  nd *= 2) daube4(f, nd, sign);
  }          
  
  public static void daube4(double f[], int n, int sign) {
    /** Daubechies 4 DWT or 1/DWT
     * @param f[]: data containing DWT or 1/DWT
     * @param n: decimation after pyramidal algorithm
     * @param sign: >=0 for DWT, @param sign:  < 0 for inverse TF    */
    double tr[] = new double [n + 1];                           // Temporary variable
    int i, j, mp, mp1; 
    if (n  <  4) return; 
    mp = n/2; mp1 = mp + 1;                          // Midpoint Midpoint +1 of array
    if (sign >= 0) {                                                           // DWT
      j = 1; 
      for ( i = 1; j <= n-3; i++ ) {                  // Smooth then detailed filters
        tr[i]      = c0*f[j] + c1*f[j + 1] + c2*f[j + 2] + c3*f[j + 3]; 
        tr[i + mp] = c3*f[j] - c2*f[j + 1] + c1*f[j + 2] - c0*f[j + 3]; 
        j += 2;                                                       // Downsampling
      }
      tr[i] = c0*f[n-1] + c1*f[n] + c2*f[1] + c3*f[2];                         // Low
      tr[i + mp] = c3*f[n-1]-c2*f[n] + c1*f[1]-c0*f[2];                       // High
    }  else  {                                                         // inverse DWT
      tr[1] = c2*f[mp] + c1*f[n] + c0*f[1] + c3*f[mp1];                        // Low
      tr[2] = c3*f[mp]-c0*f[n] + c1*f[1]-c2*f[mp1];                           // High
      for ( i=1, j=3; i < mp; i++ ) {
        tr[j] = c2*f[i] + c1*f[i + mp] + c0*f[i + 1] + c3*f[i + mp1];  
        j += 1;                                          // upsamplig  c coefficients
        tr[j] = c3*f[i]-c0*f[i + mp] + c1*f[i + 1]-c2*f[i + mp1]; 
        j += 1;                                          // upsampling d coefficients
      }                                                                        // For
       }                                                                      // Else
    for ( i=1; i <= n; i++ )   f[i] = tr[i];                      // Copy TF in array
  }                                                                         // Daube4
   
  public static double chirp(double xi) {double y; y = Math.sin(60*xi*xi); return y;}
}                                                                            // Class
