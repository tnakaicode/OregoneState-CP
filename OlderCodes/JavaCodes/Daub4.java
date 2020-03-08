/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
  /** Daub4.java 1D Daubechies 4-coefficient wavelet transform **/
import java.io.*; 
import java.util.*; 

public class Daub4 { 
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter q = new PrintWriter (new FileOutputStream("DAUB4.dat"), true); 
    System.out.println("Output to be in DAUB4.dat");        // DAUB4 wavelet 10 + 58
    int i, n = 1024; 
    double x[] = new double[n + 1];                                     
    for ( i = 1;  i <= n;  i++ )  { x[i] = 0. ; x[10] = 1. ; x[58] = 1.; }
    wt1(x, n, -1);                                                     // Inverse DWT
    for ( i = 1;  i <= n;  i++ ){q.println(" " + i + " " + x[i] + " ");}
  }

  public static void wt1(double x[], int n, int sign)  {
     // 1D DWT via pyramid algorithm, sign = +/-1, x[] replaced by transform, n = 2^N
    int nn; 
    if (n  <  4) return;                   // + Wavelet TF, - Inverse, nn >> 1 = nn/2
    if (sign >= 0) { for (nn = n; nn >= 4; nn >>= 1) daub4(x, nn, sign); }
    else {for ( nn = 4; nn <= n;  nn <<= 1) daub4(x, nn, sign); }
  }
                                           // Daubechies 4-coefficient wavelet matrix
  public static void daub4(double x[], int n, int sign)  {
    double C0 = 0.4829629131445341, C1 = 0.8365163037378079; 
    double C2 = 0.2241438680420134, C3 = - 0.1294095225512604; 
    double temp[] = new double [n + 1]; 
    int nh, nh1, i, j; 
    if ( n  <  4 ) return; 
    // quick division by 2 uses bit operator >> , nh = n>>1 = n/2
    nh1 = (nh = n >> 1) + 1; 
    if (sign >= 0) { 
			for (i = 1, j = 1; j <= n - 3; j += 2, i++) {                             // TF
        temp[i] = C0*x[j] + C1*x[j + 1] + C2*x[j + 2] + C3*x[j + 3];       
        temp[i + nh] = C3*x[j] - C2*x[j + 1] + C1*x[j + 2] - C0*x[j + 3]; 
      }
    temp[i] = C0*x[n-1] + C1*x[n] + C2*x[1] + C3*x[2];      
    temp[i + nh] = C3*x[n-1] - C2*x[n] + C1*x[1] - C0*x[2];   
    } else  {                                                          // Inverse DWT
        temp[1] = C2*x[nh] + C1*x[n] + C0*x[1] + C3*x[nh1];    
        temp[2] = C3*x[nh] - C0*x[n] + C1*x[1] - C2*x[nh1];   
        for ( i = 1, j = 3;  i  <  nh;  i++ )  {
          temp[j++] = C2*x[i] + C1*x[i+nh] + C0*x[i+1] + C3*x[i+nh1];
          temp[j++] = C3*x[i] - C0*x[i+nh] + C1*x[i+1] - C2*x[i+nh1];
        } 
      }
    for ( i = 1;  i <= n;  i++ )  x[i] = temp[i];                   // Copy temp to x
  }  
}
