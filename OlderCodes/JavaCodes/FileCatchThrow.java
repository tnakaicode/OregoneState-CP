/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// FileCatchThrow.java:  throw, catch IO exception 

import java.io.*;

public class FileCatchThrow {   

	public static void main(String[] argv)    {                           // Begin main
    double r, circum, A, PI = 3.141593;                            // Declare, assign
		r = 2;
    circum = 2.* PI* r;                                           // Calculate circum
    A = Math.pow(r,2) * PI;                                            // Calculate A
    try { 
		  PrintWriter q = new PrintWriter(new FileOutputStream("ThrowCatch.out"), true);
      q.println("r = " + r + ", length, A = " + circum + ", " +A);}  
      catch(IOException ex){ex.printStackTrace(); }                          // Catch
} }
