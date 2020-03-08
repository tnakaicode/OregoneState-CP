/*								 *
	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
	 by RH Landau, MJ Paez, and CC BORDEIANU 
	 Copyright Princeton University Press, Princeton, 2007.
	 Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
	 MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
	 Support by National Science Foundation															 
	 */
// PredatorPrey.java rk4 solution of predator & prey dynamics
import java.io.*; 

public class PredatorPrey	 { 
	
	public static void main(String[] argv) 
														 throws IOException, FileNotFoundException {
		PrintWriter w =					 // open	file .dat for output data
								new PrintWriter(new FileOutputStream("Pp40.dat"), true);
		PrintWriter q = 
								new PrintWriter(new FileOutputStream("Pp41.dat"), true);
		PrintWriter l = 
								new PrintWriter(new FileOutputStream("Pp42.dat"), true);
		double h, t; 
		double y[] = new double[3];	 
		double Tmin = 0. ;		double Tmax = 500. ;							 // Endpoints
		int Ntimes = 1000; 
		y[0] = 1.7;		y[1] = 1. ;	 y[2] = 1.7;									// Initialize
		h = (Tmax-Tmin)/Ntimes;	 
		t = Tmin; 
		for ( t = Tmin;		t <= Tmax;	t += h)	 {
			System.out.println(" t=" + t + " , x= " + y[0] + ", v= " + y[1]);
			w.println("" + t + " " + y[0] + " ");						 // Output to files
			q.println("" + t + " " + y[1] + " "); 
			l.println("" + t + " " + y[2] + " "); 
			rk4(t, y, h, 3);	 
		}
		System.out.println("Data stored in Pp2.dat"); 
	}
	
	public static void rk4(double t, double y[], double h, int Neqs)	{
		int i; 
		double F[] = new double[Neqs]; double ydumb[] = new double[Neqs];	 
		double k1[] = new double[Neqs];	 double k2[] = new double[Neqs];	
		double k3[] = new double[Neqs];	 double k4[] = new double[Neqs];	 
		f(t, y, F); 
		for ( i=0;	i < Neqs;	 i++ )	{ 
			k1[i] = h*F[i];	 
			ydumb[i] = y[i] + k1[i]/2; 
		}
		f(t+h/2, ydumb, F); 
		for ( i=0;	i < Neqs;	 i++ ) {
			k2[i] = h*F[i]; 
			ydumb[i] = y[i] + k2[i]/2; }
		f(t+h/2, ydumb, F); 
		for ( i=0;	i < Neqs;	 i++ ) {
			k3[i]= h*F[i]; 
			ydumb[i] = y[i] + k3[i]; 
		} 
		f(t+h, ydumb, F); 
		for ( i=0;	i < Neqs;	 i++ ) {
			k4[i] = h*F[i]; 
			y[i] = y[i] + (k1[i] + 2*(k2[i] + k3[i]) + k4[i])/6; 
		}
	}
	
	public static void f(double t, double y[], double F[]) { 
		 // Two Predators, one Prey model
		F[0] = 0.2*y[0]*(1-(y[0]/(1.7)))-0.1*y[0]*y[1]-0.2*y[0]*y[2]; // RHS
		F[1] = -0.1*y[1] + 0.1*y[0]*y[1];						// RHS of second equation
		F[2] = -0.1*y[2] + 0.4*y[0]*y[2]; 
	}
} 