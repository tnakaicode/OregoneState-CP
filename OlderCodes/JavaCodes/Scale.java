/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  Scale.java  scale functions for plotting Daub4 wavelets 
  import java.io.*;       
  import java.util.*; 
  
public class Scale  {
  static double c0, c1, c2, c3;                       // global coefficients forDAUB4
  static double c[] = new double[4];                            // the 4 coefficients
  static int N = 1024;                                           // matrix  dimension
  static double M[][] = new double[N][N];                    // matrix for iterations
  static double tem[] = new double[N];                              // temporary file
  static double ph[] = new double[N];                    // succesive scale functions

  public static void initial()  {                          // Initialize coefficients
    double sq3, fsq2, sq2;                                       // sqrt(3), 4sqrt(2)
    int i, j; 
    sq3 = Math.sqrt(3); 
    fsq2 = 4.*Math.sqrt(2); 
    sq2 = Math.sqrt(2);                              //1st phi function from 1st Haar
    c[0] = (1. + sq3)/fsq2;                              // Daubechies 4 coefficients
    c[1] = (3. + sq3)/fsq2; 
    c[2] = (3.-sq3)/fsq2; 
    c[3] = (1.-sq3)/fsq2; 
    ph[0] = sq2*c[0];                                                    // for x = 0
    ph[1] = sq2*c[1];                                                  // for x = 0.5
    ph[2] = sq2*c[2];                                                    // for x = 1
    ph[3] = sq2*c[3];                                                  // for x = 1.5
    for ( i=0; i < 4; i++ )  tem[i] = ph[i]; 
    for ( i=0; i < N; i++ )  { for ( j=0; j < N; j++ ) M[i][j] = 0.; }
  } // initial

  /** Does the iteration to find phi functions
   *@param : incr the increment in x (starts at 0.5, ten 0.25, 0.125 etc
   *@param : n  gives the number of values of x in one iteration
   *2param : sp gives the space between non zero diagonal in matrix
  */
  public static void sca(double incr, int n, int sp)  {
    int i, nterm, j;                              // nterm: number of nonzero columns
    double sum, sq2; 
    nterm = (n-2)/2; 
    sq2 = Math.sqrt(2); 
                // matrix in zeros to start new iteration
    for ( j=0; j < n; j++ ) { for ( i=0; i < n; i++ ) M[j][i] = 0. ; }
    for ( i=0; i < n; i++ )  {                                        // right vector
      if (i < nterm)tem[i] = ph[i];   else tem[i] = 0. ;                  // 1st != 0
    }
    for ( j=0; j < n; j++ ) {                                 // sparse matrix needed
      for ( i=0; i < nterm; i++ ) {
        if (i==j)          M[j][i] = c[0];                           // main diagonal
        if (i==(j-(sp)))   M[j][i] = c[1];                          // diagonal below
        if (i==(j-2*(sp))) M[j][i] = c[2];                  // another diagonal below
        if (i==(j-3*(sp))) M[j][i] = c[3];                     // last diagonal below
      }
    }
    for ( j=0; j < n; j++ )  {   // matrix mult for new scale function at several x's
      sum = 0. ; 
      for ( i=0; i < nterm; i++ )  sum = sum + M[j][i]*ph[i]; 
      tem[j] = sum;                                           // form temporary array
    }
    for ( i = 0; i < n; i++ )ph[i] = sq2*tem[i];                     // new iteration
  }  
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter s = new PrintWriter(new FileOutputStream("scale.dat"), true); 
    int i, n, sp, iter = 6;                           // Increase for more iterations
    double x, xi, incrx = 0.5;                                // First increment in x
    initial();                                      // Compute initial scale function
    xi = 0. ; 
    /* for ( i=0; i < 4; i++ ){               // Uncomment for 1st scale iterate func
      s.println(" " + xi + "  " + ph[i]); 
      xi = xi + incrx; 
      s.println(" " + xi + "  " + ph[i]); 
    }*/
    n = 4; 
    sp = 2;                            // Vertical space between diagonals sp = 2^n-1
    for ( i=0; i < iter; i++ )  {                            // N = 1024 for iter < 7
      incrx = incrx/2. ; 
      n = 2*n + 2;                                     // Number of x's in iterations
      sca(incrx, n, sp); 
      sp = sp*2; 
    }
    x = 0. ; 
    for ( i=0; i < n; i++ ) {                                   // Write data in file
      s.println(" " + x + "  " + ph[i]); 
      x = x + incrx; 
      s.println(" " + x + "  " + ph[i]); 
    }
    System.out.println(" Data to plot in file scale.dat"); 
  }                                                                           // Main
}                                                                            // Scale
