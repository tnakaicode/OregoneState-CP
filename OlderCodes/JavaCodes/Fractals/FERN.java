/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */

import java.io.*;                                          // Location of PrintWriter
import java.util.*;                                             // Location of Random
import java.lang.*;                                               // Location of Math

public class FERN {
  
	public static void main(String[] argv) throws IOException, FileNotFoundException {
	  PrintWriter q = new PrintWriter( new FileOutputStream("FERN.DAT"),true );
    long seed = 899432;                  // Initialize 48 bit random number generator
	  Random randnum = new Random(seed);              // For next: randnum.nextDouble()
    int imax=29999, i = 0;
	  double x = 0.5, y = 0.0, r = 0.0, xn = 0.0, yn = 0.0;
	  for (i = 1; i <= imax; i++) {
		  r = randnum.nextDouble();
      if ( r <= 0.02) { xn = 0.5;  yn = 0.27*y; }
      else if ( r > 0.02 && r <= 0.17) { xn = -0.139 * x + 0.263 * y + 0.57;  
			                                   yn =  0.246 * x + 0.224 * y - 0.036; }
      else if ( r > 0.17 && r <= 0.3) { xn =  0.17 * x - 0.215 * y + 0.408;
			                                  yn = 0.222 * x + 0.176 * y + 0.0893;  }
      else { xn = 0.781 * x +0.034 *y +0.1075; yn = -0.032 * x +0.739 *y + 0.27; }
      q.println(x+"  "+y);
		  x = xn;
		  y = yn;
		}
    System.out.println(" ");
		System.out.println("FERN Program Complete.");
		System.out.println("Data stored in FERN.DAT");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
  }
}   // End of class