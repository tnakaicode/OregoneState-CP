/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// QuantumNumerov.java: solves Schroed eq via Numerov + Bisection Algor

import java.io.*;
import ptolemy.plot.*;

public class QuantumNumerov  {

  public static void main(String[] argv) {
    Plot myPlot = new Plot();
    myPlot.setTitle("Schrodinger Eqn Numerov-Eigenfunction");
    myPlot.setXLabel("x");
    myPlot.setYLabel("Y(x)");
    myPlot.setXRange(-1000,1000);    
    myPlot.setColor(false);
    PlotApplication app = new PlotApplication(myPlot);  
    Plot myPlot1 = new Plot();
    myPlot1.setTitle("Schrodinger Eqn Numerov-Probability Density");
    myPlot1.setXLabel("x");
    myPlot1.setYLabel("Y(x)^2");
    myPlot1.setXRange(-1000,1000);    
    myPlot1.setYRange(-0.004,0.004);     
    myPlot1.setColor(false);
    PlotApplication app1 = new PlotApplication(myPlot1);  
    
    int i, istep, im, n, m, imax, nl, nr;
    double dl=1e-8, h, min, max, e, e1, de, xl0, xr0, xl, xr, f0, f1, sum, v, fact;
    double ul[] = new double[1501], ur[]  = new double[1501], k2l[]=new double[1501];
		double s[]  = new double[1501], k2r[] = new double[1501]; 
    n = 1501;    m = 5;      im = 0;    imax = 100;   xl0 = -1000;   xr0 =  1000;
    h   =  (xr0-xl0)/(n-1);
    min  = -0.001;    max = -0.00085;    e   = min;    de  = 0.01;
    ul[0] = 0.0;      ul[1] =  0.00001;  ur[0] =  0.0; ur[1] =  0.00001;
    for (i = 0; i < n; ++i)  {                             // Set up the potential k2
      xl = xl0+i*h;
      xr = xr0-i*h;
      k2l[i] = (e-v(xl));
      k2r[i] = (e-v(xr));
    }
    im = 500;                                                   // The matching point
    nl = im+2;                 nr = n-im+1;
    numerov (nl,h,k2l,ul);     numerov (nr,h,k2r,ur);
    fact= ur[nr-2]/ul[im];                                    // Rescale the solution
    for (i = 0; i < nl; ++i)  ul[i] = fact*ul[i];
    f0 = (ur[nr-1]+ul[nl-1]-ur[nr-3]-ul[nl-3])/(2*h*ur[nr-2]);           // Log deriv
    istep = 0;                                        // Bisection algor for the root
    while ( Math.abs(de) > dl && istep < imax )  {
      e1 = e;
      e  = (min+max)/2;
      for (i = 0; i < n; ++i)  {
        k2l[i] = k2l[i]+(e-e1);
	      k2r[i] = k2r[i]+(e-e1);
      }
      im=500;
      nl = im+2;               nr = n-im+1;
      numerov (nl,h,k2l,ul);   numerov (nr,h,k2r,ur);
      fact=ur[nr-2]/ul[im];                                       // Rescale solution
      for (i = 0; i < nl; ++i)  ul[i] = fact*ul[i]; 
      f1 = (ur[nr-1]+ul[nl-1]-ur[nr-3]-ul[nl-3])/(2*h*ur[nr-2]);           // Log der
      if ((f0*f1) < 0)  { max = e; de = max-min; }
      else  { min = e;  de = max-min;  f0 = f1;}
      istep = istep+1;
    }
    sum = 0;
    for (i = 0; i < n; ++i) { if (i > im) ul[i] = ur[n-i-1]; sum = sum+ul[i]*ul[i]; }
    sum = Math.sqrt(h*sum);
    System.out.println("istep=" +istep);
    System.out.println("e= "+ e+" de= "+de);
    for (i = 0; i < n; i = i+m)  {
      xl = xl0+i*h;
      ul[i] = ul[i]/sum;
      myPlot.addPoint(1, xl, ul[i], true);
      myPlot1.addPoint(1, xl, ul[i]*ul[i], true);
    }
  }

  public static void numerov (int n,double h,double k2[],double u[]){
    int i;
    for (i = 1; i <n-1; ++i)  { u[i+1]=(2*u[i]*(1-5.*h*h/12.*k2[i])
                                -(1.+h*h/12.*k2[i-1])*u[i-1])/(1.+h*h/12.*k2[i+1]); }
  }

  public static double v(double x)  {   
    double v;
    if (Math.abs(x)<=500)  v=-0.001;
    else  v=0;
    return v;
  }
}