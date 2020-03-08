/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
// Pspace JamaEigen.java: solves quantum bound state Lippmann-Schwinger eq
import Jama.*;  
import java.io.*;
    
public class PspaceJamaEigen  {
	
  public static void main(String[] argv) throws IOException, FileNotFoundException  {   
    Gauss gauss = new Gauss();
		int b = 2, i, j, g, temp3 = 0, size = 128;
    double u = .5, eps = 1.0*Math.pow(10,-8), temp = 0, temp2, test = 0;
		double lambda = -1.0, min = 0.0, max = 200.0, VR [][] = new double[size][size];
    double w[] = new double[size],  k[] = new double[size],  y[] = new double[size];
		double z[] = new double[size],  im[] = new double[size];
		Matrix H = new Matrix(size, size), Psi = new Matrix(size, size);
    Matrix ev = new Matrix(size,1), a1 = new Matrix(size,1);
    Matrix a2 = new Matrix(size,1), ground = new Matrix(size,1);
    PrintWriter q = new PrintWriter (new FileOutputStream("pspace.dat"), true);
   
    gauss.gauss(size, 0, min, max, k, w);
    for (i=0; i < size; i++) {	      	                        // Set up hamiltonian
      for(j=0; j < size; j++) {
        VR[i][j] = (lambda*b*b/(2*u))*
         	                  (Math.sin(k[i]*b)/(k[i]*b))*(Math.sin(k[j]*b)/(k[j]*b));
        if (i == j)  H.set(i, j, k[i]*k[i]/2/u +(2/Math.PI)*VR[i][j]*k[j]*k[j]*w[j]);
        H.set( i, j, (2/Math.PI)*VR[i][j]*k[j]*k[j]*w[j] );
      }
    }
    EigenvalueDecomposition E = new EigenvalueDecomposition(H);         // E = eig(H)
    y = E.getRealEigenvalues();
    im = E.getImagEigenvalues();
    for ( i = 0; i < size; i++ )  {
      System.out.println(y[i]);
      if ( y[i] < 0 )  
				{ if (Math.abs(y[i]) > Math.abs(temp)) { temp = y[i]; temp3 = i; } }
    }
    Psi = E.getV();
    for ( j = 0; j < size; j++ ) {
    	for ( i = 0; i < size; i++ ) { temp2 = Psi.get(i,j);  ev.set(i,0,temp2); }
  	  a1 = H.times(ev);
  	  a2 = ev.times(temp);	
  	  test = 0;
  	  for ( g=0;g<size;g++ ) test+=Math.abs(a1.get(g,0)-a2.get(g,0));
  		if ( test<eps ) {
  		  System.out.println("good");
  		  ground = ev;
  		  for ( g=0;g<size;g++ ) q.println(ground.get(g,0));
  		}
    }		
} }