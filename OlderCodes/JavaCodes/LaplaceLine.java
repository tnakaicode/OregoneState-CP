/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
/* LaplaceLine.java:   Laplace eqn via finite difference mthd 
                          wire in a grounded box, Output for 3D gnuplot */
import java.io.*; 

public class LaplaceLine  {
  static int  Nmax = 100;                                   // Size of box
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    double V[][] = new double[Nmax][Nmax]; 
    int i, j, iter; 
    PrintWriter w = new PrintWriter(new FileOutputStream("LaplaceLine.dat"), true);
    for (i=0;  i<Nmax;  i++) for (j=0;  j<Nmax;  j++) V[i][j] = 0.;     // Initialize
    for ( i=0;  i < Nmax;  i++ ) V[i][0] = 100. ;                  // V[i][0] = 100 V
    for ( iter=0;  iter < 1000;   iter++ ) {                            // Iterations
      for ( i=1;  i < (Nmax-1);  i++ )  {                              // x-direction
        for ( j=1;  j < (Nmax-1);  j++ )                               // y-direction
          { V[i][j] = (V[i+1][j] + V[i-1][j] + V[i][j+1] + V[i][j-1])/4.; }
      }
    }
    for ( i=0;  i < Nmax ;  i=i + 2)  {                     // Data in gnuplot format
      for ( j=0;  j < Nmax;  j=j + 2)  {w.println("" + V[i][j] + "");} 
      w.println("");                         // Blank line separates rows for gnuplot
    }                               
    System.out.println("data stored in LaplaceLine.dat"); 
} }                                                                // End main, class
