/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation                              
   */
/* JamaFit: JAMA matrix libe least-squares parabola fit of y(x) = b0 + b1 x + b2 x^2
            JAMA must be in same directory as program, or included in CLASSPATH    */
		
import Jama.*;  
import java.io.*;

public class JamaFit {  
	
  public static void main(String[] argv)throws IOException, FileNotFoundException {
	  PrintWriter w =  new PrintWriter(new FileOutputStream("jamafit.dat"), true);
    double []   x = {1., 1.05, 1.15, 1.32,  1.51, 1.68, 1.92};                // Data
    double []   y = {0.52, 0.73, 1.08, 1.44, 1.39, 1.46, 1.58}; 
    double [] sig = {0.1, 0.1, 0.2, 0.3, 0.2, 0.1, 0.1};
    double sig2, s, sx, sxx, sy, sxxx, sxxxx, sxy, sxxy, rhl, xx, yy;
    double [][] Sx = new double[3][3];                            // Create 3x3 array
    double [][] Sy = new double[3][1];                            // Create 3x1 array
		int Nd = 7, i;                                           // Number of data points
	  s = sx = sxx = sy = sxxx = sxxxx = sxy = sxy = sxxy = 0;
		
    for ( i=0; i <= Nd-1; i++ )  {                        // Generate matrix elements
      sig2   = sig[i]*sig[i];		s   += 1./sig2;		 			sx   += x[i]/sig2;
      sy    += y[i]/sig2;	    	rhl  = x[i]*x[i];   		sxx  += rhl/sig2;
      sxxy  += rhl*y[i]/sig2; 	sxy += x[i]*y[i]/sig2;  sxxx += rhl*x[i]/sig2;
      sxxxx += rhl*rhl/sig2;
    }        
    Sx[0][0] = s;                                                    // Assign arrays
    Sx[0][1] = Sx[1][0] = sx;
    Sx[0][2] = Sx[2][0] = Sx[1][1] = sxx;
    Sx[1][2] = Sx[2][1] = sxxx;
    Sx[2][2] = sxxxx;		Sy[0][0] = sy;		Sy[1][0] = sxy;		Sy[2][0] = sxxy;
		Matrix MatSx = new Matrix(Sx);                              // Form Jama Matrices
    Matrix MatSy = new Matrix(3, 1);
    MatSy.set(0, 0, sy);
    MatSy.set(1, 0, sxy);
    MatSy.set(2, 0, sxxy);
		Matrix B = MatSx.inverse().times(MatSy);                     // Determine inverse
    Matrix Itest = MatSx.inverse().times(MatSx);                      // Test inverse
		System.out.print( "B Matrix via inverse" );                         // Jama print
    B.print (16, 14);
    System.out.print( "MatSx.inverse().times(MatSx) " );
    Itest.print (16, 14);
		B = MatSx.solve(MatSy);                                    // Direct solution too
    System.out.print( "B Matrix via direct" );
    B.print (16,14);
		                            // Extract via Jama get & Print parabola coefficients
    System.out.println("FitParabola2 Final Results"); 
    System.out.println("\n");
    System.out.println("y(x) = b0 + b1 x + b2 x^2");
    System.out.println("\n");
    System.out.println("b0 = "+B.get(0,0));
    System.out.println("b1 = "+B.get(1,0));
    System.out.println("b2 = "+B.get(2,0));
    System.out.println("\n");
		for ( i=0; i <= Nd-1; i++ ) {                                         // Test fit
      s = B.get(0,0) + B.get(1,0) * x[i] + B.get(2,0) * x[i] * x[i];
      System.out.println("i, x, y, yfit = "+i+", "+x[i]+", "+y[i]+", "+s);
    }
    for ( i=0; i < Nd; i++)  {
    	yy=B.get(0,0)+B.get(1,0)*x[i]+B.get(2,0)*x[i]*x[i];
      w.println(" "+x[i] + "  " +yy + "  "+y[i]);
    }
    System.out.println("Output in jamafit.dat");
} }
