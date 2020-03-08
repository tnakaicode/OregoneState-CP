/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// RandNum.java: random numbers via java.util.Random.class 
import java.io.*;                                          // Location of PrintWriter
import java.util.*;                                             // Location of Random

public class RandNum  {
  public static void main(String[] argv) throws IOException, FileNotFoundException {
  PrintWriter q = new PrintWriter(new FileOutputStream("RandNum.DAT"), true); 
  long seed = 899432;                                  // Initialize 48 bit generator
  Random randnum = new Random(seed);  
  int imax = 100; int i = 0;                             
 // generate random numbers and store in data file:
  for ( i=1; i <= imax; i++ ) q.println(randnum.nextDouble());   
    System.out.println(" "); 
    System.out.println("RandNum Program Complete."); 
    System.out.println("Data stored in RandNum.DAT"); 
    System.out.println(" "); 
  } 
}                                                                     // End of class
