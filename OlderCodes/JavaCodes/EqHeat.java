/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// EqHeat.java: Solve heat equation via finite differences
import java.io.*;                                                // Import IO library
                       
public class EqHeat  {                                // Class constants in MKS units
  public static final int Nx = 11, Nt = 300;                            // Grid sizes
  public static final double Dx = 0.01, Dt = 0.1;                       // Step sizes
  public static final double KAPPA = 210.;                    // Thermal conductivity
  public static final double SPH = 900.;                             // Specific heat
  public static final double RHO = 2700.;                               // Al density

  public static void main(String[] argv) throws IOException, FileNotFoundException {
    int ix, t;  
    double T[][] = new double[Nx][2], cons;    
    PrintWriter q = new PrintWriter  (new FileOutputStream("EqHeat.dat"), true);
    for ( ix=1;  ix  <  Nx-1;  ix++ )  T[ix][0] = 100.;                 // Initialize
    T[0][0] = 0.;  T[0][1] = 0.;                                   // Except the ends
    T[Nx-1][0] = 0.;   T[Nx-1][1] = 0.; 
    cons = KAPPA/(SPH*RHO)*Dt/(Dx*Dx);                          // Integration factor
    System.out.println("constant = " + cons);     
    for ( t=1;  t <= Nt;  t++ )  {                                          // t loop
      for (ix=1; ix < Nx-1; ix++) 
		  	{T[ix][1] = T[ix][0]  + cons*(T[ix + 1][0] + T[ix-1][0]-2.*T[ix][0]);}
      if ( t%10==0 || t==1 )  {                                // Save every N steps
        for ( ix = 0; ix<Nx; ix++ ) q.println(T[ix][1]); 
        q.println();                                           // Blank line ends row
      } 
      for ( ix = 1; ix<Nx-1; ix++ ) T[ix][0] = T[ix][1];                // New to old
    }                                                                   // End t loop
    System.out.println("data stored in EqHeat.dat"); 
  }                                                                       // End main
}                                                                        // End class