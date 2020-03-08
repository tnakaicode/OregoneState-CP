/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
// Filter.java:  power spectrum, autocorrelation func  for
// function with random noise.  Check if autocorrelation = filter? 
import java.io.*;
import ptolemy.plot.*;
public class Filter
{   static final int max = 4000;                                 
    static double array[] = new double[max];
    static double ps[]=new double[max];
    static double[] Autocorr = new double[ max/2 ];
    public static void main(String[] argv)
{  int i, n, k;
   double step = 2*Math.PI/1000;
   double dftreal[] = new double[max];
   double dftimag[] = new double[max];
   function(array, max);
   autocorr(array);
   fourier(dftreal,dftimag);      
   invfourier(ps,step);
   //filter1();
    }
/*Calculate, plot power spectrum of initial function + noise using DFT */    
    public static void fourier(double dftreal[],double dftimag[])
 {  double real, imag;
    Plot powerspec = new Plot();
    int j, k;
    for (j = 0; j<max/2; j++)                      //loop for frequency index
     {  real = imag = 0.0;                       //clear variables
        for(k = 0; k<max/2; k++)                   //loop for sums
         {  real += Autocorr[k]*Math.cos((2*Math.PI*k*j)/max);
            imag += Autocorr[k]*Math.sin((2*Math.PI*k*j)/max); }
        dftreal[j] = (1/Math.sqrt(2*Math.PI))*real;
        dftimag[j] = (1/Math.sqrt(2*Math.PI))*imag;
        ps[j]=((dftreal[j]*dftreal[j] + dftimag[j]*dftimag[j])/max);
        powerspec.addPoint(0, j, (ps[j]), false);
     }
    powerspec.setTitle("Power Spectrum");
    powerspec.setXLabel("Frequency");
    powerspec.setYLabel("P(w)");
    powerspec.setXRange(-1.0, 45.0);
    powerspec.setImpulses(true, 0);
    PlotApplication app1 = new PlotApplication(powerspec);
}
/*Calculate, plot autocorrelation function using Inverse DFT 
  applied to power spectrum */    
    public static void invfourier(double ps[], double h)
    {
        double real,imag, A, t = 0.0;
    Plot autocorr = new Plot();
    int j, k;
    for (j = 0; j<max/2; j++)                      //loop for frequency index
        {
        real = imag = A = 0.0;                  //clear variables    
        for(k = 0; k<max/2; k++)                   //loop for sums
            {
          //real += dftreal[k]*Math.cos((2*Math.PI*k*j)/max) + dftimag[k]*Math.sin((2*Math.PI*k*j)/max);
          //imag += dftimag[k]*Math.cos((2*Math.PI*k*j)/max) - dftreal[k]*Math.sin((2*Math.PI*k*j)/max);
            real += Math.sqrt(ps[k])*Math.cos((2*Math.PI*k*j)/max);
            imag += - Math.sqrt(ps[k])*Math.sin((2*Math.PI*k*j)/max);
            }
        real = (1/Math.sqrt(2*Math.PI))*real;
        imag = (1/Math.sqrt(2*Math.PI))*imag;
        A = (real*real+imag*imag)/max;
        autocorr.addPoint(1, t, A, true);
        t += h;
        }
    autocorr.setTitle("InvTF(sqrt(A(tau))");
    autocorr.setXLabel("t (s)");
    autocorr.setYLabel("A(t)");
    //autocorr.setYRange(-1,11);
    PlotApplication app3 = new PlotApplication(autocorr);
    }
/* Setup and plot the initial function and the function sampled with random noise */    
public static void function(double [] array, int max)
 {
    double f[] = new double[max+1];
    int i;
    double step = 2*Math.PI/1000;
    double x = 0.;
    Plot initfunc = new Plot();
    Plot func = new Plot();
    for (i=0; i<max; i++)
    {  f[i]=1/(1+0.9*Math.sin(x));  // initial function
       array[i]=(1/(1+0.9*Math.sin(x)))+0.5*(2*Math.random()-1); // function+random noise
       initfunc.addPoint(0, x, f[i], true);
       func.addPoint(0, x, array[i], true);
       x+=step;
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
public static void filter1()
{
//Low-pass windowed - sinc filter 
 //Filters max samples with a 101 point windowed-sinc filter, 
//resulting in max-100 samples of filtered data.
double y[]=new double[max];//output
double h[]=new double[100];//filter kernel
           
double step = 2*Math.PI/1000;
int i,j;
Plot filter = new Plot();
//double fc = .14;               //Set the cutoff frequency (between 0 and 0.5)
double fc = .07;  
int m = 100;                //Set filter length (101 points)
                       //Calculate the low-pass filter kernel 
 for( i = 0; i<100;i++)
 {
   // inverse DFT of a rectangular pulse(the frequency response of the ideal low-pass filter)
   //  corresponds to a sinc function having the general form sinx/x
 //h[i]=Math.sin(2.0*Math.PI*fc*i)/i*Math.PI;
   if ((i-(m/2)) == 0)  h[i] = 2*Math.PI*fc;
   if ((i-(m/2))!= 0)   h[i] = Math.sin(2*Math.PI*fc * (i-m/2)) / (i-m/2);
   h[i] = h[i] * (0.54 - 0.46*Math.cos(2*Math.PI*i/m) );// Hamming window
 }
double sum = 0.0;                 //Normalize the low-pass filter kernel  
 for( i = 0;i<100;i++)
    { sum = sum + h[i]; }
 for( i = 0; i< 100;i++)
    { h[i] = h[i] / sum;}
 for( j = 100 ;j< max-1;j++)
 {    //Convolve the input signal & filter kernel
   y[j] = 0.0;                  
   for (i = 0; i< 100;i++)
   { y[j] = y[j] + array[j-i] * h[i]; }
 }
for(j=0;j<max-1;j++) {filter.addPoint(0, j*step, y[j], true);}
filter.setTitle("Low pass filtering Function + Noise");
filter.setXLabel("t (s)");
filter.setYLabel("y(t)");
PlotApplication app4 = new PlotApplication(filter);
}
public static void autocorr(double[]array){
//Calculate the autocorrelation function using the definition
//A(tau)=Sum [y(t)y(t+tau)] 
int i,j;    
Plot acorr = new Plot();
int limit = ( max/ 2 ) - 1;
int var = 0;
int point = 0;
double sum = 0.0;                                    // running sum
for (i=0; i <= limit; i++)  
{
        sum = 0.0;
        for(j = 0; j <= limit; j++)  
        { sum += ( array[ ( max / 2 )+ j]* array[ i + j ] ); }
        Autocorr[ i ] = sum * ( 2.0 /max);
acorr.addPoint(0, i, Autocorr[i], true);
}   
acorr.setTitle("Autocorrelation");
acorr.setXLabel("tau (s)");
acorr.setYLabel("A(tau)");
PlotApplication app7 = new PlotApplication(acorr);  
}
}
