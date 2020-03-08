/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Bound.java: Bound states in momentum space for delta shell potential
//                             uses JAMA and includes Gaussian integration
import Jama.*; 

public class Bound { 
  static double min =0., max =200., u =0.5, b =10. ;               // Class variables

  public static void main(String[] args) { 
    System.out.println("M, lambda, eigenvalue"); 
    for ( int M = 16; M <= 128; M += 8) {
      for ( int lambda = -1024;  lambda  <  0;  lambda /= 2) {
        double A[][] = new double[M][M],                               // Hamiltonian
        WR[] = new double[M], VR,                        // RE eigenvalues, potential
        k[] = new double[M], w[] = new double[M];                        // Pts & wts
        gauss(M, min, max, k, w);                           // Call gauss integration
        for ( int i=0;  i < M;   i++ )                             // Set Hamiltonian
          for ( int j=0;  j < M;   j++ ) {
           VR = lambda/2/u*Math.sin(k[i]*b)/k[i]*Math.sin(k[j]*b)/k[j];
           A[i][j] = 2/Math.PI*VR*k[j]*k[j]*w[j]; 
           if (i == j) A[i][j] += k[i]*k[i]/2/u; 
          }
        EigenvalueDecomposition E = new EigenvalueDecomposition(new Matrix(A));
        WR = E.getRealEigenvalues();                                // RE eigenvalues
     // Matrix V = E.getV();                                          // Eigenvectors
        for ( int j=0;  j < M;   j++ ) if (WR[j]  <  0) {
          System.out.println(M + " " + lambda + " " + WR[j]); 
          break;
  } } } }
  
 // Method gauss:  pts & wts for Gauss quadrature, uniform [a, b]
  private static void gauss(int npts, double a, double b, double[] x, double[] w) {
    int m = (npts + 1)/2; 
    double  t, t1, pp = 0, p1, p2, p3, eps = 3.e-10;                // eps = accuracy
    for ( int i=1;  i <= m;  i++ ) {
      t = Math.cos(Math.PI*(i-0.25)/(npts + 0.5));  t1 = 1; 
      while((Math.abs(t-t1))>=eps) {
        p1 = 1. ; p2 = 0. ; 
        for ( int j=1;  j <= npts;  j++ )
          {p3 = p2; p2 = p1; p1 = ((2*j-1)*t*p2-(j-1)*p3)/j;} 
        pp = npts*(t*p1-p2)/(t*t-1); 
        t1 = t; t = t1 - p1/pp; 
      }   
      x[i-1] = -t; x[npts-i] = t; 
      w[i-1]   = 2./((1-t*t)*pp*pp); w[npts-i] = w[i-1]; 
    } 
    for ( int i=0;  i < npts ;  i++ ) {
      x[i] = x[i]*(b-a)/2. + (b + a)/2. ; 
      w[i] = w[i]*(b-a)/2. ;     
} } }
