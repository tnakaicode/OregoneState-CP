/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
/*  JamaEigen.java: eigenvalue problem with JAMA. JAMA must be in same directory or 
   included in CLASSPATH. Uses Matrix.class; see Matrix.java or documentation    */
import Jama.*;  
import java.io.*;   

public class JamaEigen {
	
  public static void main(String[] argv) {
  double[][] I = { {2./3,-1./4,-1./4}, {-1./4,2./3,-1./4},  {-1./4,-1./4,2./3}}; 
  Matrix MatI = new Matrix(I);                          // Form Matrix from 2D arrays
  System.out.print( "Input Matrix" );
  MatI.print (10, 5);                                            // Jama Matrix print
	EigenvalueDecomposition E = new EigenvalueDecomposition(MatI); // Eigenvalue finder
  double[] lambdaRe = E.getRealEigenvalues();                    // Real, Imag eigens
  double[] lambdaIm =  E.getImagEigenvalues();                         // Imag eigens
  System.out.println("Eigenvalues: \t lambda.Re[]=" 
	                  + lambdaRe[0]+","+lambdaRe[1]+", "+lambdaRe[2]);
  Matrix V = E.getV();                                  // Get matrix of eigenvectors
  System.out.print("\n Matrix with column eigenvectors ");
  V.print (10, 5);
  Matrix Vec = new Matrix(3,1);                         // Extract single eigenvector
  Vec.set( 0, 0, V.get(0, 0) );
  Vec.set( 1, 0, V.get(1, 0) );
  Vec.set( 2, 0, V.get(2, 0) );
  System.out.print( "First Eigenvector, Vec" );
  Vec.print (10,5);
  Matrix LHS = MatI.times(Vec);                           // Should get Vec as answer
  Matrix RHS = Vec.times(lambdaRe[0]);
  System.out.print( "Does LHS = RHS?" );
  LHS.print (18, 12);
  RHS.print (18, 12);
}}
