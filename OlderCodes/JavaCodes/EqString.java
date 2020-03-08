/* From: "COMPUTATIONAL PHYSICS, 2nd Ed" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Wiley-VCH, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
   Support by National Science Foundation                            
*/
//  EqString.java: Leapfrog solution of wave equation, gnuplot output 
import java.io.*; 

public class EqString  {
  final static double rho = 0.01, ten = 40., max = 100.;

  public static void main(String[] argv) throws IOException, FileNotFoundException  {
    int i, k; 
    double x[][] = new double[101][3], ratio, c, c1;
    PrintWriter w = new PrintWriter (new FileOutputStream("EqString.dat"), true);
    c = Math.sqrt(ten/rho);                                      // Propagation speed
    c1 = c;                                                           // CFL criteria
    ratio =  c*c/(c1*c1);                          
		for ( i=0;  i < 81;   i++ ) x[i][0] = 0.00125*i;                 // Initial conds
    for ( i=81; i < 101;  i++ ) x[i][0] = 0.1-0.005*(i-80); 
	  for ( i=0;  i < 101;  i++ ) w.println("" + x[i][0] + "");    	 // First time step
	  w.println("");    	                                               
    for (i=1; i<100; i++) x[i][1] =x[i][0]+0.5*ratio*(x[i+1][0]+x[i-1][0]-2*x[i][0]);
			for ( k=1;  k < max;  k++ )  {                              // Later time steps
        for ( i=1;  i < 100;  i++ )  x[i][2] = 2.*x[i][1]
                                -x[i][0] + ratio*(x[i+1][1] + x[i-1][1] - 2*x[i][1]);
        for ( i=0;  i < 101;  i++ )  { x[i][0] = x[i][1]; x[i][1] = x[i][2]; }
        if ((k%5) == 0) {                                    // Print every 5th point
          for ( i=0;  i < 101;  i++ ) w.println("" + x[i][2] + "");     // Gnuplot 3D
          w.println("");                                    // Empty line for gnuplot
        } 
      }
      System.out.println("data in EqString.dat, gnuplot format");   
} }
