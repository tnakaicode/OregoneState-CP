/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//QMC.java: Quantum MonteCarlo Feynman path integration
import java.io.*;                                          // Location of PrintWriter
import java.util.*;                                             // Location of Random
import java.lang.*;                                               // Location of Math

public class QMC  {
  public static void main(String[] argv) throws IOException, FileNotFoundException {
  PrintWriter q = new PrintWriter(new FileOutputStream("QMC.DAT"), true);
  int N = 100, M = 101, Trials = 25000, seedTrials = 200;            // t grid,x(odd)
  double path[] = new double[N], xscale = 10.;
  long prop[]   = new long[M],   seed = 10199435; 
  for ( int count = 0; count < seedTrials*10; count +=  10) {
    Random randnum = new Random(seed + count); 
    double change = 0., newE = 0., oldE = 0. ; 
    for ( int i=0; i < N; i++ )  path[i] = 0. ;                       // Initial path
    oldE = energy(path);                                            // Find E of path
    for ( int i=0; i < Trials; i++ )  {                        // Pick random element
      int element = randnum.nextInt(N); 
      change = 1.8 * (0.5 - randnum.nextDouble()); 
      path[element] += change;                                         // Change path
      newE = energy(path);                                              // Find new E
      if ( newE > oldE && Math.exp(-newE + oldE)
        <= randnum.nextDouble() ) path[element]-=change;                    // Reject
      for ( int j=0; j < N; j++ )  {                             // Add probabilities
        element = (int)Math.round( (M-1)*(path[j]/xscale + .5) ); 
        if (element < M && element>=0)  prop[element]++ ; 
     }
     oldE = newE; 
    }                                                                       // t loop
  }                                                                      // Seed loop
  for ( int i=0; i < M; i++ )  q.println(xscale*(i-(M-1)/2) 
           + " " + (double)prop[i]/((double)Trials*(double)seedTrials));
  System.out.println(" "); 
  System.out.println("QMC Program Complete."); 
  System.out.println("Data stored in QMC.DAT"); 
  System.out.println(" "); 
  }
 
  public static double energy(double path[])  {
    int i = 0; 
    double sum = 0. ; 
    for ( i=0; i < path.length-2; i++ ) 
      {sum +=  (path[i+1] - path[i])*(path[i+1] - path[i]) ;} 
    sum +=  path[i+1]*path[i+1]; 
    return sum; 
} }                                                                      // End class
