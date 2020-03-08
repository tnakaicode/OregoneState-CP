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

public class COLUMN {
	
  public static void main(String[] argv) throws IOException, FileNotFoundException {
	  PrintWriter q = new PrintWriter( new FileOutputStream("COLUMN.DAT"),true);
    long seed = 971761;                  // Initialize 48 bit random number generator
	  Random randnum = new Random(seed);           // Next random: randnum.nextDouble()
    int max = 100000, npoints = 200;                     // Number iterations, spaces
	  int i = 0, dist = 0, r = 0, x = 0, y = 0, oldx = 0, oldy = 0;
	  double pp = 0.0, prob = 0.0;
		int hit[] = new int[200];
		for (i = 0; i< npoints; i++) hit[i] = 0;                           // Clear array
		oldx = 100;
		oldy = 0;
		for( i = 1; i <= max; i++) {
		r = (int)(npoints*randnum.nextDouble());
		x = r-oldx;
		y = hit[r]-oldy;
		dist = x*x + y*y;
		if (dist == 0) prob = 1.0;         // Sticking prob depends on x to last particle
		  else prob = 9.0/dist;                                     // nu = -2.0, c = 0.9
		  pp = randnum.nextDouble();
				if (pp < prob) {
			    if(r>0 && r<(npoints-1)) {
				    if((hit[r] >= hit[r-1]) && (hit[r] >= hit[r+1])) hit[r]++;
								else if (hit[r-1] > hit[r+1]) hit[r] = hit[r-1];
				        else hit[r] = hit[r+1];
				    oldx = r;
				    oldy = hit[r];
				    q.println(r+"  "+hit[r]);
				    }
			    }
		}
    System.out.println(" ");
	  System.out.println("COLUMN Program Complete.");
	  System.out.println("Data stored in COLUMN.DAT");
	  System.out.println(" ");
  }
}   // End of class