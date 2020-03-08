/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/*  JamaEigen2.java: eigenvalue problem with NIST JAMA
    JAMA must be in same directory, or include JAMA in CLASSPATH 
    uses Matrix.class, see Matrix.java or HTML documentation*/  
import Jama.*;  
import java.io.*;
    
public class JamaEigen2 {   
	
  public static void main(String[] argv)  {   
    double []   x = {1., 1.05, 1.15, 1.32,  1.51, 1.68, 1.92};
    double []   y = {0.52, 0.73, 1.08, 1.44, 1.39, 1.46, 1.58}; 
    double [] sig = {0.1, 0.1, 0.2, 0.3, 0.2, 0.1, 0.1};
    double sig2,  s,sx,sxx,sy,sxxx,sxxxx,sxy,sxxy, rhl;
    int n = 3;
                                         // Create 3x3 Inertia array, and then matrix
    double [][]I = { {-2.,2.,-3.}, {2.,1.,-6.}, {-1.,-2.,0.} }; 
                                                    // Form Matrix from Java 2D array
    Matrix MatI = new Matrix(I); 
    System.out.print( "I Matrix" );                 //  Print matrices via Jama print
    MatI.print (10,5);
    EigenvalueDecomposition E =  new EigenvalueDecomposition(MatI);
    double[] d = E.getRealEigenvalues();
    System.out.println("d[] = " + d[0]+", "+d[1]+", "+d[2]);
    Matrix V = E.getV();                                     // Matrix of eigenvector
    V.print (10,5);                         // Print out matrix, col's = eigenvectors
    Matrix V1 = new Matrix(3,1);
    V1.set(0,0,V.get(0,0));
    V1.set(1,0,V.get(1,0));
    V1.set(2,0,V.get(2,0));



}
}   // End of file
 
 



