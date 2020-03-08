/* From: "COMPUTATIONAL PHYSICS, 2nd Ed" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Wiley-VCH, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
//    Lagrange.java: Langrange interpolation of tabulated data

 import java.io.*;                          //Location of PrintWriter

public class Lagrange     {
	
  public static void main(String[] argv) throws IOException, 
	                                          FileNotFoundException  {  
    PrintWriter w = new PrintWriter(
		                new FileOutputStream("Lagr.dat"), true);
    PrintWriter ww = new PrintWriter(
	                   new FileOutputStream("Lagr_input.dat"), true);
    double x, y;
    int  i,j,k; int end = 9;
		                                                     //Input data
    double xin[] = {0, 25, 50, 75, 100, 125, 150, 175, 200};      
    double yin[] = {10.6, 6, 45, 83.5, 52.8, 19.9, 10.8, 88.25, 4.7};
		
    for (k=0; k<9; k++) ww.println( xin[k] +" " + yin[k]);
    for ( k=0; k<=1000;k++){
      x = k*0.2 ;
      y = inter(xin, yin, end, x);
     // System.out.println("Lagrange  x=" +x+" , y= "+y);    // to file
      w.println( x  +" "  +y);
    }
   System.out.println("Lagrange Program Complete.");
   System.out.println("Fit in Lagr.dat, input in Lagr_input.dat");
   System.out.println("Fitting curve in Lagr.dat");
  }
	
   public static double inter ( double xin[], double yin[], 
	                                         int end, double x)   {
		 double  lambda,y;  int i,j;
     y = 0.0;
     for ( i=1; i <= end; i++) {
       lambda = 1.0;
       for ( j=1; j<=end; j++) if (i != j) 
              {lambda *= ((x - xin[j-1])/(xin[i-1] - xin[j-1]));}
       y += (yin[i-1] * lambda);
      }
  return y;
} }
