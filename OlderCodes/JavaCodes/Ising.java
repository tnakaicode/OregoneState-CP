/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Ising.java: 1-D Ising model with Metropolis algorithm
import java.io.*;                                          // Location of PrintWriter
import java.util.*;                                             // Location of Random

 public class Ising {
  public static int N = 1000;                                      // Number of atoms
  public static double B = 1., mu = .33, J = .20,  k = 1., T = 100000000.;
   
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    Random randnum = new Random(500767);                     // Seed random generator
    PrintWriter q  = new PrintWriter(new FileOutputStream ("ising.dat"), false);
    int i, j, M = 5000;                                       // Number of spin flips
    double[] state = new double[N]; double[] test = state; 
    double ES = energy(state), p, ET;                         // State, test's energy
    for ( i=0 ;  i < N ;  i++ ) state[i] = -1.;                  // Set initial state
    for ( j=1 ;  j <= M ;  j++ ) {                           // Change state and test
      test = state; 
      i = (int)(randnum.nextDouble()*(double)N);                  // Flip random atom
      test[i] *= -1.; 
      ET = energy(test); 
      p = Math.exp((ES-ET)/(k*T));          
      if (p >= randnum.nextDouble())  { state = test; ES = ET;  }       // Test trial
      q.println(ES);                                         // Output energy to file
    }
    q.close(); 
  }

  public static double energy (double[] S)  {                // Method to calc energy
    double FirstTerm = 0., SecondTerm = 0. ; 
    int i;                                                           // Sum of energy
    for ( i=0 ;  i <= (N-2) ;  i++ )  FirstTerm += S[i]*S[i + 1]; 
    FirstTerm *= -J; 
    for ( i=0 ;  i <= (N-1) ;  i++ )  SecondTerm += S[i];
    SecondTerm *= -B*mu; 
    return (FirstTerm + SecondTerm); 
} }