/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Scatt.java: Soln of Lippmann Schwinger in p space for scattering

import Jama.*;	 
import java.io.*; 
import java.util.*; 

public class Scatt {		 
	public static void main(String[] argv) throws IOException, FileNotFoundException {								
		PrintWriter q = new PrintWriter(new FileOutputStream("sin2.dat"), true);
		int n, i, j, m, Row, Column, M = 300; 
		double pot, lambda, scale, ko, Temp, shift, shiftan, sin2, k2; 
		double pi = 3.1415926535897932384626, b = 10., RN1, potlast=0.0;
		double[][] F = new double[M][M]; double[] k = new double[M]; 
		double[] w = new double[M]; double[]D = new double[M]; double[] r =new double[M];
		double[] V = new double[M]; double[][]P = new double[M][M];
		double[][]L = new double[M][M]; double[][]U = new double[M][M];
		n = 26;								 scale = n/2;					pot = 0. ;
		shiftan = 0.;			 lambda =1.5;												     // Set up Gauss points
		Gauss.gauss(n, 2, 0., scale, k, w); 
		ko = 0.02; 
	  for ( m=1;m<901;m++)	{			                                 	// Set up D matrix
			k[n] = ko;			
			for ( i=0; i<= n-1; i++ ){
			D[i]=2/pi*w[i]*k[i]*k[i]/(k[i]*k[i]-ko*ko);
			}									
			D[n] = 0. ; 
			for (j=0; j <= n-1;j++) D[n]=D[n]+w[j]*ko*ko/(k[j]*k[j]-ko*ko);	
			D[n] = D[n]*(-2./pi); 
			for ( i=0; i <= n; i++ ) {		                  // Set up F matrix and V vector
				for ( j=0; j <= n; j++ )	{
				  pot = -b*b * lambda * Math.sin(b*k[i])* Math.sin(b*k[j])/(k[i]*b*k[j]*b);
					F[i][j] = pot*D[j]; 
					if (i==j)	F[i][j] = F[i][j] + 1.; 
				}
				V[i] = pot;			
			}																			           // Change arrays into matrices
			Matrix Fmat = new Matrix(F, n+1, n+1);		
			Matrix Vvec = new Matrix( n+1, 1); 
			Matrix Finv = Fmat.inverse();		
			for ( i=0; i <= n; i++ )	Vvec.set(i, 0, V[i]);																											
			Matrix R = Finv.times(Vvec);	                                 // Invert matrix
			RN1 = R.get(n, 0);											                 // Get last value of R
																			                          // Define phase shift
			shift = Math.atan(-RN1*ko);									
			sin2 = Math.sin(shift)*Math.sin(shift); 
			q.println(ko*b + "	" + sin2); 
			ko=ko+0.2*3.141592/1000.0;
		}
		System.out.println("Output in sin2.dat");
	 } 
} 