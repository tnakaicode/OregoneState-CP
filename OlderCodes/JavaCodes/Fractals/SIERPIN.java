/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */

import java.io.*;       //Location of PrintWriter
import java.util.*;     //Location of Random
import java.lang.*;     //Location of Math

public class SIERPIN  {
  
	public static void main(String[] argv) throws IOException, FileNotFoundException {
	  PrintWriter q = new PrintWriter( new FileOutputStream("SIERPIN.DAT"),true);
    long seed = 899432;   // Initialize 48 bit random number generator
	  Random randnum = new Random(seed);           // Next random: randnum.nextDouble()
    int imax = 29999, i = 0;
	  double a1 = 20.0, b1 = 20.0, a2 = 320.0, b2 = 20.0, a3 = 170.0, b3 = 280.0; 
	  double x = 180.0, y = 150.0, r = 0.0;
	
    for (i = 1; i <= imax; i++) {
	    r = randnum.nextDouble();
		  if (r <= 0.333333333333333)	{ x = 0.5 * (x + a1); y = 0.5 * (y + b1); }
      else if( r > 0.333333333333333 && r <= 0.666666666666666)	{ x = 0.5*(x+a2);
		                                                              y = 0.5*(y+b2); }
      else { x = 0.5 * (x + a3); y = 0.5 * (y + b3); }
      q.println(x+"  "+y); 		
    }
    System.out.println(" ");
		System.out.println("SIERPIN Program Complete.");
		System.out.println("Data stored in SIERPIN.DAT");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
		System.out.println(" ");
	}
}                                                                      // End of file