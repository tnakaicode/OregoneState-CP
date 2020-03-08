/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
// DLA.java: diffusion limired aggregation

import java.io.*;                                          // Location of PrintWriter
import java.util.*;                                             // Location of Random
import java.lang.*;                                               // Location of Math

public class DLA {
	
  public static void main(String[] argv) throws IOException, FileNotFoundException {
	  PrintWriter q = new PrintWriter( new FileOutputStream("DLA.DAT"), true );
    long seed = 971761;                  // Initialize 48 bit random number generator
	  Random randnum = new Random(seed);           // Next random: randnum.nextDouble()
	  double PI  =  Math.PI, angle = 0.0, rad = 180.0;
		int i =0, j =0, x =0, y =0, dist =0, dir =0, step =0, trav =0, hit =0, size =401;
		int max = 40000, mem_old[] = new int[2], grid[][] = new int[size][size]; 
		mem_old[0] = mem_old[1] = 0;
		for (i = 0; i < size; i++)  for (j = 0; j < size; j++) grid[i][j] = 0;  // Clear
    grid[200][200] = 1;	                                    // One particle in center
	  for (i = 0; i < max; i++) {                              // Choose starting point
		  hit = 0;
		  angle = 2. *PI * randnum.nextDouble();                          // Random angle
			x = (200 + (int)( rad * Math.cos(angle)) );                      // Coordinates
			y = (200 + (int)( rad * Math.sin(angle)) );
		  dist = gauss_ran(mem_old);                            // Random gaussian number
		  if ( dist < 0) step = -1;                            // Move forwards/backwards
		    else step = 1;
      trav = 0;
		  while( hit==0 && x<399 && x>1 && y<399 && y>1 && trav < Math.abs(dist) ) {
			  if ( grid[x+1][y] + grid[x-1][y] + grid[x][y+1] + grid[x][y-1] >= 1 ) 
				 { hit = 1; grid[x][y] = 1; }           // Neighbor occupied, particle sticks
				else if ( randnum.nextDouble() < 0.5 ) x += step;        // Move horizontally
			  else y += step;                                            // Move vertically
			  trav++;
			}
		}                                                                  // Print grid
    for (i = 0; i < size; i++) {
	    for (j = 0; j < size; j++) if(grid[i][j]==1) q.println(i+"  "+j); } 
    System.out.println(" ");
		System.out.println("DLA Program Complete.");
		System.out.println("Data stored in DLA.DAT");
		System.out.println(" ");
  }
                             // Box-Mueller random numbers with gaussian distribution
  public static int gauss_ran( int mem_old[]) {
	  Random randnum = new Random();                            // System clock is seed
    double fac = 0.0, rr = 0.0, r1 = 0.0, r2 = 0.0;
    if ( mem_old[1] == 0 ) {	                   // No number left from previous call
		  do  {                                     // Choose random point in unit circle
			  r1 = 2. * randnum.nextDouble() - 1.;
				r2 = 2. * randnum.nextDouble() - 1.;
				rr = r1 * r1 + r2 * r2;
			}
		  while ( rr>=1 || rr==0 );
		  fac = Math.sqrt(-2. * Math.log(rr)/rr);
			mem_old[0] = (int)(5000. * r1 * fac);                     // Save for next call
			mem_old[1] = 1;         		                                        // Set flag
      return( (int)(5000. * r2 * fac) );
		}
	  else {mem_old[1] = 0;	return mem_old[0];	}   // Return 2nd number from last call
  }
}   // End of file