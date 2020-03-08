 /* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// FT.java: File input & output example

import java.io.*;

public class FT {   
  static final int MAX = 10000;                                   // Global Constants
	
  public static void main(String[] argv) throws IOException, FileNotFoundException { 
	  BufferedReader in = new BufferedReader(new FileReader("Data.in"));
    PrintWriter out = new PrintWriter(new FileOutputStream("Data.out"), true);       
    String instring = "";                                         // Define variables
    double imag, real, input[] = new double[MAX + 1];
    int i =0, j, k;                                    // Place data into input array
		while( (instring = in.readLine()) != null )  {                  // Read till null
		  Double ins = new Double(instring);                          // String to double
			input[i] = ins.doubleValue();                               // Double to double
			i = i + 1;
			for( j = 0; j<i; j++ )  {                          // Transform, frequency loop
        real = imag = 0.0;                                              // Initialize
        for( k = 0; k<i; k++ )  {                                   // Loop over data
          real += input[k] * Math.cos( (2*Math.PI*k*j ) / i);
          imag += input[k] * Math.sin( (2*Math.PI*k*j ) / i); 
				}
        out.println(" "+j+" "+real+" "+imag);                           // File write
        } 
	    }
      out.close();                                                      // Close file
      System.out.println("Output in Data.out");
}}
