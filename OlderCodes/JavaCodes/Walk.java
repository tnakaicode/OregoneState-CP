// Walk.java Random walk simulation	 
import java.io.*;															             // Location of PrintWriter
import java.util.*;															  	            // Location of Random
import java.lang.*;																	              // Location of Math

public class Walk {

	public static void main(String[] argv) throws IOException, FileNotFoundException { 
		PrintWriter q = new PrintWriter( new FileOutputStream("Walk.dat"), true); 
		int imax = 1999, jmax = 10000, i = 0, j = 0;					
		double x = 0., y = 0., root2 = Math.sqrt(2.), r[] = new double[imax + 1]; 
		long seed = 3008157;		                             // New seeds => new sequence
		Random randnum = new Random(seed);        // To access next: randnum.nextDouble()
		for ( i=0;	i <= imax;	i++ ) r[i] = 0. ;						              // Initialize r
		for ( j=0;	j <= jmax;	j++ ) {								             // Average over trials
			x = 0. ;	y = 0. ;													  		           // Start at origin
			for ( i=0;	i <= imax;	i++ ) {					             // -root2<dx, dy< +root2
			  x += (randnum.nextDouble()-0.5)*2.*2; 
				y += (randnum.nextDouble()-0.5)*2.*root2; 
				r[i] += Math.sqrt(x*x + y*y);                                       // Radius
			} 
		}
		for ( i=0;	i <= imax;	i++ ) 
              q.println((i + 1) + "	" + Math.sqrt(i) + "	" + r[i]/((double)jmax)); 
		System.out.println(" "); 
		System.out.println("Walk Program Complete."); 
		System.out.println("Data stored in Walk.dat"); 
		System.out.println("Data Format: k, sqrt(k), r[k]"); 
		System.out.println("k = step number, k = 1 to " + (imax + 1)); 
		System.out.println("r[k] = distance from origin at step k"); 
		System.out.println("data averaged over " + jmax + " trials"); 
	} 
}		 