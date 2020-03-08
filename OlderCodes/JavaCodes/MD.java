/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// MD.java, Molecular Dyanmics via Lennard-Jones potential, velocity Verlet algorithm
import java.io.*;
import java.util.*;

public class MD { 
  static  int L, Natom = 8, Nmax = 513;                            // Class variables
  static double x[] = new double[Nmax], fx[][] = new double[Nmax][2];
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    int t1, t2, i, Itemp, t, Nstep=5000, Nprint=100, Ndim=1;
    double h = 0.0004, hover2, PE, KE, T, Tinit = 10.0, vx[] = new double[Nmax]; 
    L = (int)Math.pow(1.*Natom, 1./Ndim); 
    Natom =  (int)Math.pow(L, Ndim);
    System.out.println("Natom = "+Natom+" L= "+L+"");
    i = -1;
    for ( int ix = 0; ix <= L-1; ix++ ) {                 // Set up lattice of side L
      i = i+1;
      x[i] = ix;                                                // Initial velocities
      vx[i] =(Math.random()+Math.random()+Math.random()+Math.random()+Math.random()
              +Math.random()+Math.random()+Math.random()+Math.random()+
                   Math.random()+Math.random()+Math.random())/12.-0.5; 
      vx[i] = vx[i]*Math.sqrt(Tinit);                     // Scale v with temperature
      System.out.println("init vx = "+vx[i]);
    } 
    t1 = 0;    t2 = 1;                                              // t, t+h indices
    hover2 = h/2.;
    t = 0;
    KE = 0.0;  PE = 0.0;                                         // initial KE & PE v
    PE = Forces(t1, PE);
    for ( i = 0; i <= Natom-1; i++ )    KE=KE+(vx[i]*vx[i])/2;
    System.out.println(t+" PE= "+PE+" KE = "+KE+" PE+KE = "+(PE+KE));
    for( t = 1; t < Nstep; t++ ) {
      for( i = 0; i <= Natom-1; i++ )  {                                 // Main loop
        PE = Forces(t1, PE);                                       // Velocity Verlet
        x[i] = x[i] + h*(vx[i] + hover2*fx[i][t1]);
        if (x[i] <= 0.) x[i] = x[i] + L;                                       // PBC
        if (x[i] >= L)  x[i] = x[i] - L;
      }
    PE = Forces(t2, PE);
    KE = 0.; 
    for( i = 0; i <= Natom-1; i++)  { 
      vx[i] = vx[i] + hover2*(fx[i][t1] + fx[i][t2]);
      KE = KE + (vx[i]*vx[i])/2; 
    }
    T = 2.*KE / (3.*Natom); 
    if (t%Nprint==0)System.out.println(t+" PE ="+PE+" KE = "+KE+" PE+KE = "+(PE+KE));
    Itemp = t1;                                                     // Time t and t+h
    t1 = t2;         t2 = Itemp;
    }
   }
                                                            // Force = class variable
  public static double Forces(int t, double PE) { 
    int i, j;
    double fijx, r2, invr2=0, dx, r2cut = 9.;
    PE = 0.;                                                            // Initialize
    for (i=0; i<= Natom-1; i++)   {fx[i][t] = 0.; } 
    for(i = 0; i<= Natom-2; i++)  { 
      for(j = i+1; j<=Natom-1; j++) { 
        dx = x[i]-x[j];
        if (Math.abs(dx) > 0.50*L) {dx = dx - sign(L,dx);}                     // PBC
        r2 =  dx*dx ;
        if (r2 < r2cut)  {                                                 // Cut off
          if ( r2 == 0.) r2 = 0.0001;
          invr2 = 1./r2;   
          fijx =  48.*(Math.pow(invr2,3)-0.5)*Math.pow(invr2,3);    
          fijx = fijx*invr2*dx;
          fx[i][t] = fx[i][t] + fijx;
          fx[j][t] = fx[j][t] - fijx;
          PE = PE + 4*Math.pow(invr2,3)*( Math.pow(invr2,3) - 1.);
        }
      } 
    }
  return PE;    
  }
  
  public static double  sign(double a,double b) 
    {if (b >= 0.) return Math.abs(a);  else  return -Math.abs(a); }      
}
