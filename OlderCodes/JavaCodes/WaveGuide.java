/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// WaveGuide.java  FDTD solution of Maxwell's equations in 3-D

import java.io.*;
import java.util.*;
public class WaveGuide{

 public static void main(String[] argv) throws IOException, FileNotFoundException{
   PrintWriter el =new PrintWriter(new FileOutputStream("Electric.dat"), true);
   PrintWriter mag =new PrintWriter(new FileOutputStream("Magnetic.dat"), true);
	 double eps0 = 8.8541878e-12, mu0  = 4e-7 * Math.PI;  // Permittivity, permeability
	 double c0   = 299792458;                               // Speed of light in vacuum
	 int i, j, k, n, Nt = 128;                                  // Number of time steps
	 double Lx = .05, Ly = .04, Lz = .03;                // Cavity dimensions in meters
	 int Nx =  25, Ny =  20,  Nz =  15;                   // N of cells in ea direction
	 double Cx = Nx / Lx, Cy = Ny / Ly, Cz = Nz / Lz;        // Inverse cell dimensions
	 double nrm = 866.0254, Dt = 1/(c0*nrm);                               // Time step	
	 double Ex[][][] = new double [Nx+1][Ny+2][Nz+2];         //Allocate field matrices
	 double Ey[][][] = new double [Nx+2][Ny+1][Nz+2];
	 double Ez[][][] = new double [Nx+2][Ny+2][Nz+1];
	 double Hx[][][] = new double [Nx+2][Ny+1][Nz+1];
	 double Hy[][][] = new double [Nx+1][Ny+2][Nz+2];
	 double Hz[][][] = new double [Nx+1][Ny+1][Nz+2]; 
	 double Et[][]   = new double [Nt][3];                     // Allocate time signals
	 Ex[1][2][2] = 1.;                         // Initial fields near (not on) boundary
	 Ey[2][1][2] = 2.;
	 Ez[2][2][1] = 3.;
	 for (n = 1; n < Nt; n++) {                   // Time stepping, Update H everywhere
		 for (i = 1; i <Nx+1; i++) {for (j = 1; j <Ny; j++) { for (k = 1; k < Nz; k++) {
					Hx[i][j][k] = Hx[i][j][k]+(Dt/mu0)*((Ey[i][j][k+1]
						              - Ey[i][j][k]) * Cz - (Ez[i][j+1][k]-Ez[i][j][k]) * Cy); }
     }  }
     for (i = 1; i < Nx+1; i++){ for (j = 1; j < Ny; j++){ for (k = 1;k<Nz;k++) { 
					 Hy[i][j][k] = Hy[i][j][k] + (Dt/mu0)*((Ez[i+1][j][k]
							                 -Ez[i][j][k])*Cx - (Ex[i][j][k+1]-Ex[i][j][k])*Cz); }
     } }
     for (i = 1; i < Nx+1; i++) {for (j = 1; j < Ny; j++) {for (k = 1; k < Nz; k++){
					 Hz[i][j][k] =Hz[i][j][k]+(Dt/mu0)*((Ex[i][j+1][k]
							                  -Ex[i][j][k])*Cy - (Ey[i+1][j][k]-Ey[i][j][k])*Cx); }
     } }                                                 // Update E, except boundary
    for (i = 1; i < Nx; i++) { for (j = 2; j < Ny; j++) {for (k = 2; k < Nz; k++){
					 Ex[i][j][k] = Ex[i][j][k] + (Dt /eps0)*((Hz[i][j][k]
					                       -Hz[i][j-1][k])*Cy-(Hy[i][j][k]-Hy[i][j][k-1])*Cz);}
    } }
    for (i = 1; i < Nx; i++) { for (j = 2; j < Ny; j++){ for (k = 2; k < Nz; k++){
				 Ey[i][j][k] = Ey[i][j][k] + (Dt/eps0)*((Hx[i][j][k]-Hx[i][j][k-1])*Cz
				                                          -(Hz[i][j][k]-Hz[i-1][j][k])*Cx); }
    } }
    for (i = 1; i < Nx; i++) { for (j = 2; j < Ny; j++){ for (k = 2; k < Nz; k++){
					Ez[i][j][k] = Ez[i][j][k] + (Dt/eps0)*((Hy[i][j][k]-Hy[i-1][j][k])*Cx
					                                        -(Hx[i][j][k]-Hx[i][j-1][k])*Cy); }
    } } 
  }                                                                    // n loop ends
  for (i = 1; i < Nx+1; i++) { for (j = 1; j < Ny; j++){ for (k = 1; k < Nz; k++){
				mag.println("  "+ Hx[i][j][k] +"  "+Hy[i][j][k] +" "+Hz[i][j][k]);    }
	} }                                                                   // H printout
  for (i = 1;i < Nx; i++){ for ( j = 2; j < Ny; j++) { for (k = 2; k < Nz; k++){
        el.println("   "+ Ex[i][j][k]+ "  "+ Ey[i][j][k] +"  "+Ez[i][j][k]); }
  } }                                                                   // E printout
  }
}
