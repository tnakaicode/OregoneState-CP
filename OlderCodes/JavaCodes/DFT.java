/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//DFT.java: Discrete Fourier Transform
import java.io.*;  

public class DFT {
  static final int N = 1000, Np = N;                              // Global constants
  static double [] signal = new double[N + 1]; 
  static double twopi = 2.*Math.PI, sq2pi = 1./Math.sqrt(twopi); 
  static double h = twopi/N;
   
  public static void main(String[] argv)  {   
    double dftreal[] = new double[Np], dftimag[] = new double[Np]; 
    f(signal); 
    fourier(dftreal, dftimag);       
  }
                   
  public static void fourier(double dftreal[], double dftimag[]) {
    double real, imag;                                            // Calc & plot Y(w)
    int n, k; 
    for ( n = 0;  n < Np;  n++ ) {                               // Loop on frequency
      real = imag = 0. ;                                           // Clear variables
      for ( k = 0;  k < N;  k++ ){                                   // Loop for sums
        real += signal[k]*Math.cos((twopi*k*n)/N); 
        imag += signal[k]*Math.sin((twopi*k*n)/N); 
      }
      dftreal[n] = real*sq2pi; 
      dftimag[n] = -imag*sq2pi; 
    }
  }
  
  public static void f(double [] signal) {                        // Initial function
    int i; 
    double step = twopi/N, x = 0.; 
    for ( i=0;  i <= N;  i++ ) { signal[i] = 5.+10*Math.sin(x+2.);  x += step; }
  }
}
