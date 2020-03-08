/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// FDTD.java  FDTD solution of Maxwell's equations in 1-D
import java.io.*;
import java.util.*;

public class FDTD {
  public static void main(String[] argv) throws IOException, FileNotFoundException  {
    double dx, dt, beta, c0;
		int time = 100, max = 200, i, n, j, k;
		beta = 0.5;                             // beta = c/(dz/dt) < 0.5 for   stability
		double Ex[] = new double[max+1]; double Hy[] = new double[max+1];      // Ex & Hy
                   //   Gaussian Pulse Variables
	  int m = max/2;                 											  // Pulse in center of space
		double t = 40, width = 12.;                         // Center, width of the pulse
		PrintWriter w =new PrintWriter( new FileOutputStream("E.dat"), true );
		PrintWriter q =new PrintWriter( new FileOutputStream("H.dat"), true );
			// Intial conditions
		for( k = 1; k < max; k++ ) Ex[k] = Hy[k]=0.;
				
		for ( n = 0; n < time; n++ ) {                               
      for( k = 1; k < max; k++ ) Ex[k]  = Ex[k] + beta * ( Hy[k-1]-Hy[k] );   //Eq 1
      Ex[m] =  Ex[m] + Math.exp( -0.5*((t-n)/width)*((t-n)/width) );         // Pulse
      for( j = 0; j < max-1; j++) Hy[j] = Hy[j] + beta * ( Ex[j]-Ex[j+1] );   // Eq 2
    }
    for ( k = 0; k < max; k++ ) {
 	    w.println(""+k+" "+Ex[k]+" " +Hy[k]+" ");
      q.println(""+k+" "+Hy[k]+" ");
    }   
} }    