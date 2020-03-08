/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// twoplates.java:  Navier-Stokes solution for the laminar flow 
//                  between 2 plates via successive over-relaxation
import java.io.*;
import java.util.*;

public class twoplates {
 
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter q  = new PrintWriter (new FileOutputStream("twopr.dat"), true);
    int i, j, iter, Nxmax = 400,  Nymax = 40;                      // Grid parameters
    double  V0 = 1.0, err, r1, r2, omega;                         // Initial velocity
    double vx[][] = new double [Nxmax+1][Nymax+1];                           
    double vy[][] = new double [Nxmax+1][Nymax+1];                              
    double P[][] =  new double [Nxmax+1][Nymax+1];                         
    omega = 0.53; 
    System.out.println("omega =  "+omega+"");
    iter = 0;                                                 // Number of iterations
    err = 1.0;                                                           // Precision
    while ((err>0.1) && (iter  <= 100))  {
      err = 0.0;
      for (i = 0; i <= Nxmax-1; i++) {vx[i][Nymax-1]=0; vy[i][Nymax-1]=0;}  // Top BC
      for( j = 0; j <= Nymax-1; j++) { vx[0][j] = V0; vy[0][j] = 0; }     // Inlet BC
      for( i = 0; i  <= Nxmax-1; i++ ) { vx[i][0] = 0; vy[i][0] = 0; }   // Bottom BC
      for( j = 0; j  <= Nymax-1; j++ )                                   // Outlet BC
        { vx[Nxmax-1][j] = vx[Nxmax-2][j];  vy[Nxmax-1][j] = vy[Nxmax-2][j]; }
      for ( i = 1;i  <= Nxmax-1; i++ )  {
        for ( j = 1; j  <= Nymax-1; j++ )  {
          vx[i+1][j] = vx[i-1][j] + vy[i][j-1] - vy[i][j+1];
          r1 = omega*(vx[i+1][j] + vx[i-1][j] + vx[i][j+1] + vx[i][j-1] - 4*vx[i][j] 
                     - 0.5*vx[i][j]*(vx[i+1][j]-vx[i-1][j])
                     - 0.5*vy[i][j]*(vx[i][j+1]-vx[i][j-1])+0.5*(12.0/400.))/4.;
          vx[i][j] +=  r1;
          r2 = omega*(vy[i+1][j] + vy[i-1][j] + vy[i][j+1] + vy[i][j-1]-
            4*vy[i][j] - 0.5*vx[i][j]*(vy[i+1][j] - vy[i-1][j])-
            0.5*vy[i][j]*(vy[i][j+1] - vy[i][j-1]))/4.0;
          vy[i][j] +=  r2;                                                     // Top
          if (i>= 0 && i <= Nxmax-1 && j == Nymax-1) {vx[i][j] = 0; vy[i][j] = 0;} 
          if (i>= 0 && i  <= Nxmax-1 && j == 0) {vx[i][j] = 0; vy[i][j] = 0;}  // Bot
          if ((i == Nxmax-1) && (j>= 0) && (j<= Nymax-1))  
                {vx[i][j] = vx[i-1][j];   vy[i][j] = vy[i-1][j]; }             // Out
          if (i == 0 && j>= 0 && j<= Nymax-1)  {vx[i][j] =V0; vy[i][j] =0;}  // Inlet
          err = Math.max(err, Math.abs(r1));
        }
      }
      iter++;
    }
    for (i = 0;i<= Nxmax-1;i++ ) {
      for (j = 0;j<= Nymax-1;j++) 
        { q.println(" iter =  "+iter+" j =  "+j+" vx =  "
                + vx[i][j]+" vxe =  "+6.0*(j/40.0)*(1.0-(j/40.0))+""); }
      q.println("");
    } 
} } 
