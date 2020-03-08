/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Laplace.java:  Solve Laplace eqn with finite difference mthd        
import java.io.*; 

public class Laplace  { 
  static int  Nmax = 100;                                   // Size of box

  public static void main(String[] argv) throws IOException, FileNotFoundException {
    double V[][] = new double[Nmax][Nmax]; 
    int i, j, iter; 
    PrintWriter w =  new PrintWriter  (new FileOutputStream("Laplace.dat"), true); 
     //    Inititize array     
    for (i=0; i < Nmax; i++) {for (j=0; j < Nmax; j++) V[i][j] = 0.;}
        for ( i = 25;  i <= 75;  i++ ) { V[i][37] = -100.;  V[i][63] = 100.;}   // BC
        for ( i = 0;  i <  Nmax;  i++ )  {                           // Box potential
          V[0][i] = 0. ; 
          V[Nmax-1][i] = 0. ;  
          V[i][0] = 0. ; 
          V[i][Nmax-1] = 0. ; 
        } 
    for ( iter = 0;  iter <  1000;  iter++ ) {                     // 1000 iterations
      for ( i = 1;  i <= Nmax-2;  i++ ) {                              // x-direction
        for ( j = 1;  j <= Nmax-2;  j++ ) {                            // y-direction
          V[i][j] = 0.25*(V[i+1][j] + V[i-1][j] + V[i][j+1] + V[i][j-1]);
          if (j==37 && i>=25 && i <= 75) V[i][j] = 100. ;                  // Fixed V
          if (j==63 && i>=25 && i <= 75) V[i][j] = -100. ; 
        }
      }
    }
    for ( i=0;  i < Nmax ;  i=i + 2) {        // Data in gnuplot 3D format
      for ( j=0;  j < Nmax;  j=j + 2)  w.println("" + V[i][j] + ""); 
        w.println("");            // Blank line separates rows for gnuplot
    }
    System.out.println("data stored in Laplace.dat"); 
  }                                                                       // End main
}                                                                        // End class
