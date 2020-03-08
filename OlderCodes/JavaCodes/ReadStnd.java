/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
   Support by National Science Foundation
   */
// ReadStnd.java: standard (keyboard) input stream 
import java.io.*;             //Import I/O library 

public class ReadStnd { 
  public static final double PI = Math.PI;                                //Constants
  public static void main(String[] argv) throws IOException  {                // main
  double r, A;                                                    // Throws exception
  BufferedReader b = new BufferedReader(new InputStreamReader(System.in)); 
  System.out.println("Please enter your name");                              // Input
  // Read & print 1st line; read & convert 2nd line to double
  String name = b.readLine();
  System.out.println("Hi "+name+" \n Please enter r");
  String rS = b.readLine();
  r = Double.valueOf(rS).doubleValue();    
  A = PI * r * r;                                                  // The computation
  System.out.println("\n Bye "+name+"\t R= " +r+", A=" +A);  
  }
}
