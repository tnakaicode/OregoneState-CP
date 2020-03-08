/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
import java.io.*; 
import ptolemy.plot.*; 
// NoiseSincFilte.java Calculate & plot power spectrum + noise using DFT  

public class NoiseSincFilter  {
  static final int max = 4000;                                  
  static double array[] = new double[max], ps[] = new double[max]; 
  
  public static void main(String[] argv)  {
    int i, n, k; 
    double step = 2*Math.PI/1000; 
    double dftreal[] = new double[max], dftimag[] = new double[max]; 
    function(array, max);                    // Fourier(dftreal, dftimag), invfourier
    filter1(); 
  }
  
  public static void fourier(double dftreal[], double dftimag[])  {   
    double real, imag; 
		int j, k; 
    Plot powerspec = new Plot(); 
    for ( j = 0;  j < max;  j++ )  {                            // Loop for frequency
      real = imag = 0. ;                                           // Clear variables
      for ( k = 0;  k < max;  k++ )   {                              // Loop for sums
        real += array[k]*Math.cos((2*Math.PI*k*j)/max); 
        imag += array[k]*Math.sin((2*Math.PI*k*j)/max); 
      }
      dftreal[j] = (1/Math.sqrt(2*Math.PI))*real; 
      dftimag[j] = (1/Math.sqrt(2*Math.PI))*imag; 
      ps[j] = Math.abs(dftreal[j]*dftreal[j]+dftimag[j]*dftimag[j])/max;
      powerspec.addPoint(0, j, ps[j], false); 
    }
    powerspec.setTitle("Power Spectrum"); 
    powerspec.setXLabel("Frequency"); 
    powerspec.setYLabel("P(w)"); 
    powerspec.setXRange(-1., 45.); 
    powerspec.setImpulses(true, 0); 
    PlotApplication app1 = new PlotApplication(powerspec); 
  }
  
 // Calculate & plot autocorrelation function using Inverse DFT 
  public static void invfourier(double ps[], double h)  {
    double real, imag, A, t = 0. ; 
    Plot autocorr = new Plot(); 
    int j, k; 
    for ( j = 0;  j < max;  j++ )  {                            // Loop for frequency
      real = imag = A = 0. ;                                       // Clear variables
        for ( k = 0;  k < max;  k++ )  {                             // Loop for sums
          /* real += dftreal[k]*Math.cos((2*Math.PI*k*j)/max) 
                        + dftimag[k]*Math.sin((2*Math.PI*k*j)/max); */
          /* imag += dftimag[k]*Math.cos((2*Math.PI*k*j)/max) 
                        - dftreal[k]*Math.sin((2*Math.PI*k*j)/max); */
          real += ps[k]*Math.cos((2*Math.PI*k*j)/max); 
          imag += - ps[k]*Math.sin((2*Math.PI*k*j)/max); 
        }
        A = Math.sqrt(real*real + imag*imag); 
        autocorr.addPoint(1, t, A, true); 
        t += h; 
    }
    autocorr.setTitle("Autocorrelation function"); 
    autocorr.setXLabel("t (s)"); 
    autocorr.setYLabel("A(t)");                        // Autocorr.setYRange(-1, 11);
    PlotApplication app3 = new PlotApplication(autocorr); 
  }
  
  public static void function(double [] array, int max)  {
    double f[] = new double[max + 1]; 
    int i; 
    double step = 2*Math.PI/1000, x = 0.; 
    Plot initfunc = new Plot(); 
    Plot func = new Plot(); 
    for ( i=0;  i < max;  i++ )  {  
      f[i] = 1/(1. + 0.9*Math.sin(x));                            // Initial function
                 // function + random noise
      array[i] = (1./(1.+ 0.9*Math.sin(x))) + 0.5*(2*Math.random()-1.); 
      initfunc.addPoint(0, x, f[i], true); 
      func.addPoint(0, x, array[i], true); 
      x += step; 
    }
    initfunc.setTitle("Initial Function"); 
    initfunc.setXLabel("t (s)"); 
    initfunc.setYLabel("f(t)"); 
    PlotApplication app3 = new PlotApplication(initfunc); 
    func.setTitle("Initial Function + Noise"); 
    func.setXLabel("t (s)"); 
    func.setYLabel("y(t)"); 
    PlotApplication app2 = new PlotApplication(func); 
 }

  public static void filter1() {                   // Low-pass windowed - sinc filter
    double y[] = new double[max], h[] = new double[100], step = 2*Math.PI/1000; 
    int i, j, m = 100;                              // Set filter length (101 points)
    Plot filter = new Plot(); 
    double fc = .07;   
    for ( i = 0;  i < 100; i++ )  {               // Calculate low-pass filter kernel
      if ((i-(m/2)) == 0)  h[i] = 2*Math.PI*fc; 
      if ((i-(m/2))!= 0)   h[i] = Math.sin(2*Math.PI*fc*(i-m/2))/(i-m/2);
      h[i] = h[i]*(0.54 - 0.46*Math.cos(2*Math.PI*i/m));             //Hamming window
    }
    double sum = 0. ;                             // Normalize low-pass filter kernel
    for ( i = 0; i < 100; i++ )  sum = sum + h[i];
    for ( i = 0;  i <  100; i++ )  h[i] = h[i] / sum; 
    for ( j = 100 ; j <  max-1; j++ )  {                // Convolve input with filter
      y[j] = 0. ;                   
      for ( i = 0;  i <  100; i++ )  y[j] = y[j] + array[j-i] * h[i];
    }
    for ( j=0; j < max-1; j++ ) filter.addPoint(0, j*step, y[j], true); 
    filter.setTitle("Low pass filtering Function + Noise"); 
    filter.setXLabel("t (s)"); 
    filter.setYLabel("y(t)"); 
    PlotApplication app4 = new PlotApplication(filter); 
  }
}
