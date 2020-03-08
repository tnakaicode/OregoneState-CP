/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/
// NoiseMP2.java:  Power spectrum of function + noise  
import java.io.*;
import ptolemy.plot.*;
public class Noise1MP{
    static final int max =1000;                                 
    static double array[] = new double[max], ps[]=new double[max];
    static double Autocorr[]=new double[max], twopi=2*Math.PI;
    static double step= 4*Math.PI/1000;   
 public static void main(String[] argv){
      double dftreal[]=new double[max], dftimag[]=new double[max];
      discrete(array,max);
      autocorr(array);
      fourier(dftreal,dftimag);
      filter1();       }
 public static void fourier(double dftreal[],double dftimag[]){
      double real, imag;
      Plot powerspec = new Plot();
      int n, k;
      for (n = 0; n<max; n++) {          //loop for frequency index
        real = imag = 0.0;               //clear variables
        for(k = 0; k<max; k++){         //loop for sums  
                real += Autocorr[k]*Math.cos((twopi*k*n)/max);
                imag += Autocorr[k]*Math.sin((twopi*k*n)/max);  }
        dftreal[n] = (1/Math.sqrt(twopi))*real;
        dftimag[n] = (1/Math.sqrt(twopi))*imag;
        ps[n]=(Math.abs(dftreal[n]*dftreal[n] + dftimag[n]*dftimag[n])/max);
        powerspec.addPoint(0, n, ps[n], false);
        }
        powerspec.setTitle("Power Spectrum");
        powerspec.setXLabel("Frequency");
        powerspec.setYLabel("P(w)");
        powerspec.setXRange(-1.0, 45.0);
        powerspec.setImpulses(true, 0);
      PlotApplication app1 = new PlotApplication(powerspec);  }
 public static double func(double t)
    { return  1.0/(1.0+0.9*Math.sin(t));}
// Setup, plot function +- random noise  
   public static void discrete(double [] array, int max){
     double f[] = new double[max+1];
     int i;
     double t = 0.;
     Plot initfunc = new Plot();
     Plot func = new Plot();
     for (i=0; i<max; i++){   // function +- noise
       f[i]=/*1/(1+0.9*Math.sin(t))*/ func(t);
       array[i]=/*(1/(1+0.9*Math.sin(t)))*/ f[i]+0.5*(2*Math.random()-1);
       initfunc.addPoint(0, t, f[i], true);
       func.addPoint(0, t, array[i], true);
       t+=step;                                   }
    initfunc.setTitle("Initial Function");
    initfunc.setXLabel("t (s)");
    initfunc.setYLabel("f(t)");
    PlotApplication app3 = new PlotApplication(initfunc);
    func.setTitle("Initial Function + Noise");
    func.setXLabel("t (s)");
    func.setYLabel("y(t)");
    PlotApplication app2 = new PlotApplication(func);
}
  public static void filter1(){
    //Low-pass windowed-sinc filter,
    // 101 point windowed-sinc filter results in max-100 output 
    double y[]=new double[max];//output
    double h[]=new double[100];//filter kernel
    int i, n;
    Plot filter = new Plot();
    double fc = .07;        //  Set cutoff frequency (between 0 & 0.5)
    int m = 100;            //  Set filter length (101 points)      
   for( i = 0; i<100;i++){  //Calculate the low-pass filter kernel 
   //inverse DFT of rectangular pulse, f(t) = sinc  =  sin x/x
     if ((i-(m/2)) == 0)  h[i] = twopi*fc;
     if ((i-(m/2))!= 0)   h[i] = Math.sin(twopi*fc * (i-m/2)) / (i-m/2);
     h[i] = h[i] * (0.54 - 0.46*Math.cos(twopi*i/m) );// Hamming window
   }
  double sum = 0.0;                 //Normalize low-pass filter kernel  
  for( i = 0;i<100;i++){                                  
    sum = sum + h[i];  }
 for( i = 0; i< 100;i++){
   h[i] = h[i] / sum;   }
 for( n = 100 ;n< max-1;n++){    // Convolute & filter
   y[n] = 0.0;                  
   for (i = 0; i< 100;i++){
     y[n] = y[n] + array[n-i] * h[i];    }    }
 for(n=0;n<max-1;n++){filter.addPoint(0, n*step, y[n], true);}
  filter.setTitle("Low pass filtering Function + Noise");
  filter.setXLabel("t (s)");
  filter.setYLabel("y(t)");
  PlotApplication app4 = new PlotApplication(filter);
}
public static void autocorr(double[]array){
  //Calculate autocorrelation function  = integral [y(t)y(t+tau)] 
  int i,n;  
  double tau=0.,t,sum;
  Plot acorr = new Plot();
  for(i=0;i<max; i++){
    sum=0.0;
    t=0.0;
    for(n=0;n<max-1;n++){   //trap integration rule
      sum +=(func(t)*func(t+tau)+func(t+step)*func(t+step+tau))*step/2.0;
      t +=step;          }   
    Autocorr[ i ] = sum;
    acorr.addPoint(1, tau, Autocorr[i], true);
    tau += step;         }
  acorr.setTitle("Autocorrelation");
  acorr.setXLabel("tau (s)");
  acorr.setYLabel("A(tau)");
  PlotApplication app7 = new PlotApplication(acorr);    
 } } 