/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                               
   */
// CWT_zd.java  Continuous Wavelet Transform. Written by Zlatko Dimcovic
// Outputs transform.dat = TF, recSigNorm.dat =TF^{-1}  
import java.io.*; 

public class CWTzd  {                                    // N.B. many class variables
	public static final double PI = Math.PI;
	public static double iT = 0.0, fT = 12.0, W = fT - iT;                 // i,f times
	public static int N = 1000; public static double h = W/N;                   //Steps
	public static int noPtsSig = N, noS = 100, noTau = 100;                 // # of pts
	public static double iTau = 0., iS = 0.1, tau = iTau, s = iS;
	// Need *very* small s steps for high-frequency, but only if s is small
	// Thus increment s by multiplying by number close enough to 1 
	public static double dTau = W/noTau, dS = Math.pow(W/iS, 1./noS);
	
	public static void main(String[] args) throws IOException, FileNotFoundException  {
	  System.out.printf("\nUsing:\n\ttau + dTau, dTau = %3.3f (noTau = %d)"
			+ "\n\t s*dS, dS = %3.3f (noS = %d)%n%n", dTau, noTau, dS, noS);
		String transformData = "transform.dat";                              // Data file
		double[] sig = new double[noPtsSig];                                    // Signal
		signal(noPtsSig, sig, false);
		double[][] Y = new double[noS][noTau];                               // Transform
		for (int i = 0; i < noS; i++, s *= dS) {                               // Scaling
			tau = iT;
			for (int j = 0; j < noTau; j++, tau+=dTau)                       // Translation
				Y[i][j] = transform(s, tau, sig);
		}                                                  // Print normalized TF to file
		PrintWriter wd = new PrintWriter( new FileWriter(transformData), true);
		double maxY = 0.001;
		for (int i = 0; i < noS; i++)
			for (int j = 0; j < noTau; j++)
				if( Y[i][j]>maxY || Y[i][j]<-1*maxY ) 
					maxY = Math.abs( Y[i][j] );                                   // Find max Y
 		tau = iT; s = iS;
		for (int i = 0; i < noS; i++, s*=dS) {                              // Write data
			for (int j = 0; j < noTau; j++, tau+=dTau) {                       // Transform
				wd.println(s + " " + tau + " " + Y[i][j]/maxY); }              // Norm to max
			tau = iT;
			wd.println();                                                 // For gnuplot 3D
		}
		wd.close();
		                                                               // Find inverse TF
		String recSigData = "recSig.dat";                   
		PrintWriter wdRecSig=new PrintWriter(new FileWriter(recSigData),true);
		double[] recSig = new double[sig.length];                      // Same resolution
		double t = 0.0;
		for (int rs = 0; rs < recSig.length; rs++, t += h) {
			recSig[rs] = invTransform(t, Y);
			wdRecSig.println(t + " " + recSig[rs]);                           // Write data
		}
		wdRecSig.close();
		System.out.println("\nDone.\n");  }                                   // End main
  
	public static double transform(double s, double tau, double[] sig)  {
		double integral = 0., t = iT;                  // "initial time" = class variable
 		for (int i = 0; i < sig.length; i++, t+=h) integral += sig[i]*morlet(t,s,tau)*h;
		return integral / Math.sqrt(s);  }

	public static double invTransform(double t, double[][] Y)  {
		double s = iS, tau = iTau, recSig_t = 0;                 // Don't change static's
		for (int i = 0; i < noS; i++, s *= dS) {
			tau = iTau;
			for (int j = 0; j < noTau; j++, tau += dTau) 
				recSig_t += dTau*dS * Math.pow(s,-1.5)* Y[i][j] * morlet(t,s,tau);
		}
		return recSig_t; }

	public static double morlet(double t, double s, double tau) {             // Mother
	  double T = ((t-tau)/s);
		return Math.sin(8*T) * Math.exp( -T*T/2 );
	}
	 
 	public static void signal(int noPtsSig, double[] y, boolean plotIt) {
		double t = 0.0; double hs = W / noPtsSig;
		for (int i = 0; i < noPtsSig; i++, t+=hs) {
			double t1 = W/6., t2 = 4.*W/6.;       
			if ( t>= iT && t<=t1 ) y[i] = Math.sin(2*PI*t);
			else if ( t>=t1 && t<=t2 ) y[i] = 5.*Math.sin(2*PI*t) + 10.*Math.sin(4*PI*t);
			else if ( t>=t2 && t<=fT )
				y[i]=2.5*Math.sin(2*PI*t)+6.*Math.sin(4*PI*t)+10*Math.sin(6*PI*t);
			else {
				System.err.println("\n\tIn signal(...) : t out of range.\n");
				System.exit(1);
			} } }

  public static void setParameters(int ptsTransf) {N=ptsTransf; h=W/N; noPtsSig = N;}
		
 	public static void setParameters(int ptsTau, int ptsS) {noTau = ptsTau; noS = ptsS;
		dTau = W/noTau; dS = Math.pow(W/iS, 1./noS); }
		
	public static void setParameters(int ptsTransf, int ptsTau, int ptsS) {
		N = ptsTransf; h = W/N; noPtsSig = N;
		noTau = ptsTau; noS = ptsS;
		dTau = W/noTau; dS = Math.pow(W/iS, 1./noS); }
	 }                                                                     // End class
