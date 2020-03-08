/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// ReadStnd2.java: parse input for int, double

import java.io.*;  

public class ReadStnd2  {   
	public static final double PI = Math.PI;            
  
	public static void main(String[] argv) throws IOException, FileNotFoundException {
    double r, A;                                                // Variables instants
    BufferedReader b=new BufferedReader(new InputStreamReader(System.in));
    System.out.println("Please enter your age");                             // Input
    String ageS = b.readLine();                                               // Read
    int age = Integer.parseInt(ageS);                               // Convert to int
    System.out.println("Hi at "+age+" yrs,\n Please enter r");
    String rS = b.readLine();                                                 // Read
    r = Double.parseDouble(rS);                                  // Convert to double
    A = PI * r * r;
    System.out.println("\n r= " +r+", A= " +A);                             // Screen
  }
}
