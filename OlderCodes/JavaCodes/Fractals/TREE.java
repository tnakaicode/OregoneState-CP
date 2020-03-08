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

public class TREE {
	
  public static void main(String[] argv) throws IOException,  FileNotFoundException {
	  PrintWriter q = new PrintWriter( new FileOutputStream("TREE.DAT"),true );
	  long seed = 899432;                         //Seed 48 bit random number generator
	  Random randnum = new Random(seed);              // For next: randnum.nextDouble()
    int imax=9999, i=0;
	  double x = 0.0, y = 0.5, r = 0.0, xn = 0.0, yn = 0.0;
	  
		for (i = 1; i <= imax; i++) {
		  r = randnum.nextDouble();
      if( r <= 0.1 ) { xn = 0.05 * x;  yn = 0.6 * y; }
      else if( r > 0.1 && r <= 0.2 ) { xn = 0.05 * x;  yn = -0.5 * y + 1.; }
      else if( r > 0.1 && r <= 0.2 ) { xn = 0.05 * x;  yn = -0.5 * y +1.0; }
      else if( r > 0.2 && r <= 0.4 ) { xn = 0.46*x -0.32*y;  yn = 0.39*x+0.38*y+0.6;}
      else if (r > 0.4 && r <= 0.6 ) { xn = 0.47*x-0.15*y;   yn = 0.17*x+0.42*y+1.1;}
      else { xn = 0.42 * x + 0.26 * y;  yn = -0.35 * x + 0.31 * y + 0.7;}
      q.println(xn+"  "+yn);
		  x = xn;
		  y = yn;
    }
    System.out.println(" ");
		System.out.println("TREE Program Complete.");
		System.out.println("Data stored in TREE.DAT");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
  }
}                                                                     // End of class