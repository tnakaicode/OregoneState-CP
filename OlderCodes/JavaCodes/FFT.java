/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
/* FFT.java:  FFT for complex numbers dtr[][]
              data1[i][0], data1[i][1] = Re, Im parts of point [i].
              When done, Re, Im Fourier Transforms placed in same array 
              Required max = 2^m < 1024
              dtr[][] placed in array data[]:  */ 
import java.util.*; 
import java.io.*; 

public class FFT  {
  public static int max = 2100;                                   // Global variables
  public static int points = 1026;                                // Can be increased
  public static double data[] = new double[max]; 
  public static double dtr[][] = new double[points][2];
  
  public static void main(String[] argv)  {
    int isign, i, nn = 16;                                              // Power of 2                                      
    isign = -1;                                 // -1 transform, +1 inverse transform
    for ( i=0; i<nn; i++ )  {                                           // Form array
      dtr[i][0] = (double)(i);                                             // Re part
      dtr[i][1] = (double)(i);                                             // Im part
      System.out.println("dtr " + dtr[i][0] + " im " + dtr[i][1]);
    }
    fft(nn, isign);                                   // Call FFT, use global dtr[][]
    for(i=0; i <nn; i++) System.out.println("i "+i+" FT " +dtr[i][0]+"  "+dtr[i][1]);
  }
  
  public static void fft(int nn, int isign) {                     // FFT of dtr[n][2]
    int i, j, m, n, mmax, istep;
    double tempr, tempi, wr, wi, wstpr, wstpi, theta, sinth;
    n = 2*nn;
    for( i = 0; i <= nn; i++ )  {                     // Original data in dtr to data
      j = 2*i + 1;
      data[j] = dtr[i][0];                                   // Real dtr, odd data[j]
      data[j+1] = dtr[i][1];                              // Imag dtr, even data[j+1]
      System.out.println(" Input data ");
      System.out.println(" dt "+j+"d  "+data[j]+"  "+ data[j+1]);
    }
    j = 1;                                         // Place data in bit reverse order
    for ( i = 1; i <= n; i=i+2 )  {
      if( (i-j)<0 )  {                           // Reorder equivalent to bit reverse
        tempr = data[j];
        tempi = data[j+1];
        data[j] = data[i];
        data[j+1] = data[i+1];
        data[i] = tempr;
        data[i+1] = tempi; 
      }   
      m = n/2;
      do { if ( (j-m)<=0 )   break; j = j-m; m = m/2; }
      while ( m-2>0 ); j = j+m;
    }                                          //  Print data and data to see reorder
    System.out.println(" Bit-reversed data ");
    for (i=1; i<=n; i=i+2) System.out.println (" i " + i + " data[i] "+data[i]);
    mmax = 2;
    while( (mmax-n)<0 )  {                                         // Begin transform
      istep = 2*mmax;
      theta = 6.2831853/(float)(isign*mmax);
      sinth = Math.sin(theta/2.0);
      wstpr = -2.0*sinth*sinth;
      wstpi = Math.sin(theta);
      wr = 1.0;      wi = 0.0;
      for (m=1; m <= mmax; m=m+2)  {
        for(i=m; i<=n; i=i+istep)  {
          j = i+mmax;
          tempr = wr*data[j]-wi*data[j+1];
          tempi = wr*data[j+1]+wi*data[j];
          data[j] = data[i]-tempr;
          data[j+1] = data[i+1]-tempi;
          data[i] = data[i]+tempr;
          data[i+1] = data[i+1]+tempi;
        }                                                                    // For i
        tempr = wr;
        wr = wr*wstpr-wi*wstpi+wr;
        wi = wi*wstpr+tempr*wstpi+wi;
      }                                                                      // For m
      mmax = istep;
    }                                                                        // While
    for(i=0; i<nn; i++)  {j = 2*i+1; dtr[i][0] = data[j]; dtr[i][1] = data[j+1]; } 
} }                                                                
