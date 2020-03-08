/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Decay.java: Spontaneous decay simulation 
import java.io.*; 
import java.util.*;
                 
public class Decay {
  static double lambda = 0.01;                                      // Decay constant
  static int max = 1000, time_max = 500, seed = 68111;                      // Params
   
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    int atom, time, number, nloop; 
    double decay; 
    PrintWriter w = new PrintWriter(new FileOutputStream("decay.dat"), true); 
    number = nloop = max;                                            // Initial value
    Random r = new Random(seed);                             // Seed number generator
    for ( time = 0;  time <= time_max;  time++ ) {                       // Time loop
      for ( atom = 1;  atom <= number;  atom++ ) {                      // Decay loop
        decay = r.nextDouble();   
        if (decay  <  lambda) nloop--;                                     // A decay
    }                                                           
      number = nloop; 
      w.println( " " + time + "  " + (double)number/max); 
    }
   System.out.println("data stored in decay.dat"); 
} } 