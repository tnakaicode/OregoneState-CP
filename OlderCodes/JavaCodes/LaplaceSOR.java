/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// LaplaceSOR.java: Solves Laplace eqn using SOR for convergence

import java.io.*; 


public class LaplaceSOR  { 
  static int  max =  40;                                     // Number of grid points

  public static void main(String[] argv) throws IOException, FileNotFoundException  {
	  double x, tol, aux, omega, r, pi  =  3.1415926535, p[][] = new double[max][max];
    int i, j, iter;
	  PrintWriter w =new PrintWriter(new FileOutputStream("laplR.dat"), true);
    omega = 1.8;                                                     // SOR parameter
    long timeStart=System.nanoTime();
    System.out.println(""+timeStart+"");
	  for (i=0; i<max; i++)   for ( j=0; j<max; j++ ) p[i][j]  =  0.;           // Init
    for (i = 0; i<max; i++) p[i][0]  =  +100.0;
    tol  =  1.0;                                                          // Tolerance
    iter  =  0; 
	  while ( (tol > 0.000001) && (iter <=  140) ) {                      // Iterations
		  tol  =  0.0;
		  for (i = 1; i<(max-1); i++)  {                         
        for (j = 1; j<(max-1); j++)   {                              // SOR ALGORITHM
				  r  =  omega *(p[i][j+1] + p[i][j-1] + p[i+1][j] + p[i-1][j] - 4*p[i][j])/4;
          p[i][j] +=  r;
          if ( Math.abs(r) > tol ) tol  =  Math.abs(r);
        }
        iter++;
      }
    }
    long timeFinal=System.nanoTime();
    System.out.println(""+timeFinal+"");
    System.out.println("Nanoseconds="+(timeFinal-timeStart));
    for (i = 0; i<max ; i++)  {            
      for (j = 0; j<max; j++) w.println(""+p[i][j]+"");  
      w.println("");                                       //  empty line for gnuplot
    }
    System.out.println("data stored in laplR.dat");
}  } 