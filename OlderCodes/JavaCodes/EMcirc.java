/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  EMcirc.java: FDTD Propgation of circularly polarized EM wave
import java.io.*;
import java.util.*;

public class EMcirc {
	
  public static double Exini( int tim, double x, double phx)            // Initial Ex
		{  return Math.cos(tim-2*Math.PI*x/200. +phx ); }
	public static double Eyini( int tim, double x, double phy)            // Initial Ey
    {  return Math.cos(tim-2*Math.PI*x/200. +phy ); }
 public static double Hxini( int tim, double x, double phy)              // Inital Hx
   {  return Math.cos(tim-2*Math.PI*x/200. +phy+Math.PI); }
 public static double Hyini( int tim, double x, double phx)             // Initial Hy
    { return Math.cos(tim-2*Math.PI*x/200.0 +phx); }
 
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    double dx, dt, beta, c0, phx, phy;
		int time = 100, max = 200, i, n, j, k;
		phx = 0.5*Math.PI; phy = 0.0;                        // Phase, difference is pi/2
		beta = 0.1;                               // beta = c/(dz/dt) < 0.5 for stability        
		double Ex[] = new double[max+1]; double Hy[] = new double[max+1];       // Ex, Hy
		double Ey[] = new double[max+1]; double Hx[] = new double[max+1];       // Ey, Hx
		for (i=0; i < max; i++) {Ex[i]=0;  Ey[i]=0;  Hx[i]=0;  Hy[i]=0;}    // Initialize
		for(k = 0; k < max; k++)                                        // Initialize
      { Ex[k]=Exini(0,(double)k,phx);  Ey[k]=Eyini(0,(double)k,phy);
        Hx[k]=Hxini(0,(double)k,phy);  Hy[k]=Hyini(0,(double)k,phx);   }
    PrintWriter w = new PrintWriter( new FileOutputStream( "Efield.dat" ), true);
    PrintWriter q = new PrintWriter( new FileOutputStream( "Hfield.dat" ), true);
    for (n = 0; n < time; n++ ) {     
      for(k = 1; k < max; k++ ) {                                       // New Ex, Ey
       Ex[k] = Ex[k] + beta * (Hy[k-1] - Hy[k]);  
		   Ey[k] = Ey[k] + beta * (Hx[k]   - Hx[k-1]);
      }  
      for (j = 0; j < max-1; j++ ) {                                    // New Hx, Hy
        Hy[j] = Hy[j] + beta * (Ex[j]   - Ex[j+1]);    
        Hx[j] = Hx[j] + beta * (Ey[j+1] - Ey[j]);
      }      
    }                                                                         // Time
    for ( k = 0; k < max; k = k+4 ) {                          // Plot every 4 points
      w.println(""+0.0+" "+0.0+" "+k+"  "+Ex[k]+" "+Ey[k]+"  "+0.0);
      q.println(""+0.0+" "+0.0+" "+k+"  "+Hx[k]+" "+Hy[k]+"  "+0.0);
} } }    
