/*                 *
   From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC Bordeianu, Univ Bucharest, 2007.
   Support by National Science Foundation                              
*/
// Bugs.java: Bifurcation diagram for logistic map (sorted) 
import java.io.*;

public class Bugs {     
  static float m_min =0.0f, m_max =4.f, step =.01f ;    // Class variables

  public static void main(String[] argv) 
                    throws IOException, FileNotFoundException { 
  float  m;   float y[]=new float[1000]; 
  int i;
  PrintWriter w =                         // Output data to BugsSorted.dat
        new PrintWriter(new FileOutputStream("BugsSorted.dat"), true);
  for ( m = m_min; m  <= m_max; m += step) {                    // mu loop
    y[0] = 0.5f;                           /* arbitrary seed */
    for (i=1;   i <=200; i++ ) y[i] = m*y[i-1]*(1-y[i-1]);   // transients
    for (i=201; i <=401; i++ ) y[i] = m*y[i-1]*(1-y[i-1]);   // Record 200
    for (i=201; i <=401; i++ ) {                           // Remove dupes
      if (y[i]!=y[i-1]) {                   
        y[i] = m*y[i-1]*(1-y[i-1]);
        w.println( ""+ m+" "+ y[i]);}
      }
  }
  System.out.println("sorted data stored in BugsSorted.dat.");
  }
}
