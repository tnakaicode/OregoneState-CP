/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
   Support by National Science Foundation                              
*/
// WaveletSmall.java: Continuous wavelet TF using Morlet wavelets 
import java.io.*;
import ptolemy.plot.*;

public class WaveletSmall	 {                                  // Many class variables
	public static final double PI = Math.PI;
	public static double iT = 0.0, fT = 12.0, W = fT - iT;                 //i, f times
	public static int N = 1000, noPtsSig = N, noS = 100, noTau = 100;          
	public static double iTau = 0.0, iS = 0.1, tau = iTau, s = iS, h = W/N; // Stepsize
	// Need *very* small steps for high-f, s -> s*number >=1, changes large s most  
	public static double dTau = W/noTau, dS = Math.pow(W/0.1, 1./noS);
	
  public static void main(String[] args) throws IOException, FileNotFoundException {
    String usageMessage = "\nUsage: Wavelets [parameters]. Params are:\n"
			+ "\n\t(0) None, run with defaults; shown below [in square brackets]"
			+ "\n\t(1) Number of points for calculating TF integral [1000]"
			+ "\n\t(2) Number of steps for: time shift (tau), scaling (s) [" 
				+ noTau + " " + noS + "]"
			+ "\n\t(3) All: N noTau noS [1000 " + noTau + " " + noS + "]\n";
		if ( args.length == 0 ) { System.out.println(
		"\nNo arguments, use defaults.(Run with -h for usage message.)\n"); }
		else if ( args.length == 1 && args[0].startsWith("-h") ) {
			System.err.println( usageMessage ); System.exit(1); }
		else if ( args.length == 1 ) setParameters( Integer.parseInt(args[0]) ); 
		else if ( args.length == 2 ) {setParameters(Integer.parseInt(args[0]),
		                                   Integer.parseInt(args[1]));}
		else if (args.length == 3) { setParameters( Integer.parseInt(args[0]),
			Integer.parseInt(args[1]), Integer.parseInt(args[2]) ); }
		else { System.err.println( usageMessage ); System.exit(1); }
		System.out.printf("Using:\n\ttau + dTau, with dTau=%3.3f (noTau=%d)"
			+ "\n\t s	 * dS, with dS=%3.3f (noS=%d)%n%n", dTau, noTau, dS, noS);
		String WaveletTF = "WaveletTF.dat";									                 // Data file
		double[] sig = new double[noPtsSig];	                 // For signal to work with
		signal(noPtsSig, sig, false);					                  // Calculate (don't plot)
		plotWavelets(tau, s);									                  // Plot mother wavelet(s)
		double Ystau = 0;		                               // Find the transform Y(s,tau)
		double[][] Y = new double[noS][noTau];									             // Transform
		for (int i = 0; i<noS && s<W; i++, s *= dS)	{ 					           // Scan scales
			for (int j = 0; j<noTau && tau<=fT; j++, tau+=dTau)	{	            // Time shift
				Ystau = transform(s, tau, sig);
				Y[i][j] = Ystau;
			}
			tau = iT;
		}
		PrintWriter wd = new PrintWriter( new FileWriter(WaveletTF), true);
		double maxY = 0.001;                                                 // Normalize
		for (int i = 0; i < noS; i++) for (int j = 0; j < noTau; j++)
		if( Y[i][j]>maxY || Y[i][j]<-1*maxY ) maxY = Math.abs( Y[i][j] );
		tau = iT; s = iS;
		for (int i = 0; i < noS; i++, s*=dS) {
			for (int j = 0; j < noTau; j++, tau+=dTau) 
                                 {wd.println(s + " " + tau + " " + Y[i][j]/maxY);}
			wd.println();	                                          // For gnuplot  3D plot
			tau = iT;
		}
		wd.close();
		plotCompare(sig);                  // Plot signal; instructor also can do inverse
	 	System.err.println("\nDone.\n");
	}                                                                       // End main
 
	// Find TF: integrate wavelet*signal over t (near peak) for fixed s, tau
	  public static double transform(double s, double tau, double[] sig) {
		double integral = 0.; double t = iT;                      // "initial t" (static)
		for (int i = 0; i < sig.length; i++, t+=h) 
                    {integral += sig[i] * morlet(t, s, tau)* h/ Math.sqrt(s);}
		return integral;
	}
                                                                             //Morlet
	public static double morlet(double t, double s,double tau) {double T = ((t-tau)/s);
		return Math.sin(8*T) * Math.exp( -T*T/2 ); }
	 
  public static void signal(int noPtsSig, double[] y, boolean plotIt) {
		double t = 0.0, h = W / noPtsSig;
		for (int i = 0; i < noPtsSig; i++, t+=h) {
			if ( t>= 0 && t<=2 )      y[i] = Math.sin(2*PI*t);					 
			else if ( t>=2 && t<=8 )  y[i] = 5.*Math.sin(2*PI*t) + 10.*Math.sin(4*PI*t);
			else if ( t>=8 && t<=12 ) y[i] = 2.5*Math.sin(2*PI*t) + 6*Math.sin(4*PI*t)
				                                + 10*Math.sin(6*PI*t);
			else {System.err.println("\n\t signal() : t out of range.\n"); System.exit(1);}
		  if (plotIt) {										                           // Show if asked for
			  Plot ptSig = new Plot();
			  for (i = 0; i < noPtsSig; i++, t+=h) ptSig.addPoint(0, i*h, y[i], true);
			  ptSig.setXLabel("time"); ptSig.setYLabel("signal");
			  ptSig.setTitle("Signal: 1 or 2 or 3 sin's over 3 intervals");
			  PlotApplication showSig = new PlotApplication(ptSig);
		 }
	}
}

	// Set class variables: number of points for the transform.  
	public static void setParameters(int ptsTransf) 
	 {N = ptsTransf; h = W/N; noPtsSig = N; }
	
	// Set class variables: No  points for time translation, scale.  
	public static void setParameters(int ptsTau, int ptsS)  {noTau = ptsTau; 
    noS = ptsS; dTau = W/noTau; dS = Math.pow(W/0.1, 1./noS); }
		
	// Set class variables: No points for TF, time translation, scale.  
	public static void setParameters(int ptsTransf, int ptsTau, int ptsS)  {
		N = ptsTransf; h = W/N; noPtsSig = N;
		noTau = ptsTau; noS = ptsS;
		dTau = W/noTau; dS = Math.pow(W/0.1, 1./noS);  }

	public static void plotCompare(double[] sig) {
	  double maxSig = 0.001, maxInv = 0.001, t = 0.0;
		for (int i = 0; i < sig.length; i++) if ( sig[i] > maxSig || sig[i] < -1*maxSig )
			                                        maxSig = sig[i];
		Plot plComp = new Plot();
		for (int i = 0; i < sig.length; i++, t+=h) plComp.addPoint(0, t, sig[i], true); 
		plComp.setXLabel("time"); plComp.setYLabel("signal");
		plComp.setTitle("Signal");
		plComp.addLegend(0, "Signal");	
		PlotApplication shComp = new PlotApplication(plComp);
	}
			
	public static void plotWavelets(double tau, double s)  {             // Plot Mother
	  Plot ptMorlet = new Plot();  
		for (double t=-W/2.; t < W/2.; t+=h) ptMorlet.addPoint(0,t,morlet(t,s,tau),true);
		String morletTitle = "Morlet wavelet (tau=" + tau + ", s=" + s + ")";
		ptMorlet.setXLabel("time"); ptMorlet.setYLabel("Psi");
		ptMorlet.setTitle( morletTitle );
		PlotApplication shMorlet = new PlotApplication( ptMorlet );
	}
}                                                                        // End class
