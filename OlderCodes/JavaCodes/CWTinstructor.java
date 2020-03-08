/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
// CWTcommented.java  Continuous Wavelet Transform, with full comments
// Written by Zlatko Dimcovic

/* This program writes files:
 *
 *  signal.dat     :  input signal 
 *  transform.dat  :  wavelet transform (3 columns), normalized to 1
 *  recSig.dat     :  inverse transform = reconstituted signal
 *  recSigNorm.dat :  normalized reconstructed signal   
 *
 *  Also written is: gnuplotBatchFile.gp, which is used at run-time for the
 *  gnuplot invokation in batch mode ("batch file"), to plot the 3D transform. 
 */
 
import java.io.*;
import ptolemy.plot.*;

/** Use Morlet, Mexican Hat or Haar wavelets to analyze a few simple signals. 
 *  Find, save data, and plot the transform, and inverse transform (to check).
 *  Judged by the reconstructed signal, the Morlet wavelet handles these test
 *  signals best. (While other wavelets seem reasonable too.)
 */
public class CWTinstructor
{
	// This class is used as a stand-alone program; we use many class variables.

	public static final double PI = Math.PI;

	// Time: initial, final, the interval width.
	public static double iT = 0.0, fT = 12.0, W = fT - iT;
	
	// Number of points and step size for the transform integral and signal.
	public static int N = 1000; public static double h = W/N;
	public static int noPtsSig = N;         // same (good for reconstruction)
	
	// Time translation, scaling: number of points, initial values, increments.
	public static int noS = 100, noTau = 100;
	public static double iTau = 0.0, iS = 0.1, tau = iTau, s = iS;
	// To pick high-frequency behavior, it is critical to have *very* small steps
	// for s -- but only when s is small. Thus we increment s by multiplying it by
	// a number close enough to 1; then small numbers are changed little, and so s
	// is varied more finely when small.  (For the defaults above, dS = 1.049)
	public static double dTau = W/noTau, dS = Math.pow(W/iS, 1./noS);
	
	public static void main(String[] args) throws IOException, FileNotFoundException
	{
		String usageMessage = "\nUsage: java Wavelets [parameters]. The parameters are:\n"
			+ "\n\t(0) None, run with defaults; shown below [in square brackets]"
			+ "\n\t(1) Number of points for calculating transform integral [" + N + "]"
			+ "\n\t(2) Number of steps for: time translation (tau), scaling (s) [" 
				+ noTau + " " + noS + "]"
			+ "\n\t(3) All of the above: N noTau noS [" + N + " " + noTau + " " + noS + "]\n";

		if ( args.length == 0 ) { System.out.println(
			"\nNo arguments, go with defaults. (Run with -h for the usage message.)");
		}
		else if ( args.length == 1 && args[0].startsWith("-h") ) {
			System.err.println( usageMessage ); System.exit(1);
		}
		else if ( args.length == 1 ) {
			setParameters( Integer.parseInt(args[0]) ); 
		}
		else if ( args.length == 2 ) {
			setParameters( Integer.parseInt(args[0]), Integer.parseInt(args[1]) );
		}
		else if ( args.length == 3 ) {
			setParameters( Integer.parseInt(args[0]), Integer.parseInt(args[1]), 
				Integer.parseInt(args[2]) );
		}
		else { System.err.println( usageMessage ); System.exit(1); }

		System.out.printf("\nUsing:\n\ttau + dTau, with dTau = %3.3f (noTau = %d)"
			+ "\n\t s  * dS,   with dS   = %3.3f (noS   = %d)%n%n",  
			dTau, noTau, dS, noS);

		String transformData = "transform.dat";                  // data file name

		double[] sig = new double[noPtsSig];  // For the signal to work with;
		signal(noPtsSig, sig, false);         // calculate it (don't plot).

		plotWavelets(tau, s);                 // Plot (show) mother wavelet(s).
		
	
		/* Find the transform */
		
		double[][] Y = new double[noS][noTau];                   // transform

		for (int i = 0; i < noS; i++, s *= dS)                   // scaling
		{	
			tau = iT;
			for (int j = 0; j < noTau; j++, tau+=dTau)             // translation
				Y[i][j] = transform(s, tau, sig);
		}
	
		
		/* Print transform data to file, normalized (to 1) */
		
		PrintWriter wd = new PrintWriter( new FileWriter(transformData), true);
		double maxY = 0.001;
		for (int i = 0; i < noS; i++)
			for (int j = 0; j < noTau; j++)
				if( Y[i][j]>maxY || Y[i][j]<-1*maxY ) 
					maxY = Math.abs( Y[i][j] );                        // find max Y
	
 		tau = iT; s = iS;
		for (int i = 0; i < noS; i++, s*=dS) {                   // Write all data
			for (int j = 0; j < noTau; j++, tau+=dTau) {           // for transform,
				wd.println(s + " " + tau + " " + Y[i][j]/maxY);      // norm'ed to max.
			}
			tau = iT;
			wd.println();                       // keep gnuplot happy (3D plot)
		}
		wd.close();

		
		/* Reconstruct the signal (find inverse transform). NOT normalized. */
		
		String recSigData = "recSig.dat";
		PrintWriter wdRecSig = new PrintWriter( new FileWriter(recSigData), true);

		double[] recSig = new double[sig.length];                // same resolution
		double t = 0.0;
		for (int rs = 0; rs < recSig.length; rs++, t += h)
		{
			recSig[rs] = invTransform(t, Y);
			wdRecSig.println(t + " " + recSig[rs]);                // write data
		}
		wdRecSig.close();

		plotCompare(sig, recSig);  // Plot signal and normalized inverse together
			 
		System.out.println("\nDone.\n");

	} // end of main

	/* The first two methods below compute the transform and its inverse using the 
	 * Morlet wavelet. To use others, change the morlet() call to mexhat(), haar().
	 */

	/** Find transform: integrate wavelet*signal over time, for given s, tau.
	 * Note: for optimization it should integrate only over the wavelet's 
	 * width; outside of it the wavelet, and the integral, are negligible.
	 */
	public static double transform(double s, double tau, double[] sig)
	{
		double integral = 0.;
		double t = iT;                       // "initial time" (class variable)
		
		for (int i = 0; i < sig.length; i++, t+=h)
			integral += sig[i] * morlet(t, s, tau) * h;
		
		return integral / Math.sqrt(s);
	}

	/** Inverse transform (reconstruct the signal): integrate over s, tau. */
	public static double invTransform(double t, double[][] Y)
	{
		// Don't want static s, tau to be changed when this function finishes;
		// so we declare variables local to this block (masking global s,tau).
		// These are concerns typical for using global variables (too freely).
		double s = iS, tau = iTau;
		double recSig_t = 0;

		for (int i = 0; i < noS; i++, s *= dS)
		{
			tau = iTau;
			for (int j = 0; j < noTau; j++, tau += dTau) 
				recSig_t += dTau*dS * Math.pow(s,-1.5) * Y[i][j] * morlet(t,s,tau);
		}
		return recSig_t;
	}

	/** Morlet mother wavelet. */
	public static double morlet(double t, double s, double tau)
	{
		double T = ((t-tau)/s);

		return Math.sin(8*T) * Math.exp( -T*T/2 );
	}
	/** Mexican Hat mother wavelet. */
	public static double mexhat(double t, double s, double tau)
	{
		double T = ((t-tau)/s);
		
		return (1-T*T) * Math.exp( -T*T/2. );
	}
	/** Haar mother wavelet. */
	public static double haar(double t, double s, double tau)
	{
		double psi = 0.0;

		if      ( t >= tau-s && t <  tau   ) psi = 1.;
		else if ( t >  tau   && t <= tau+s ) psi = -1.;
		else psi = 0.;

		return psi;
	}

	/** Computes the signal, with the given number of points (and can show it). */
	public static void signal(int noPtsSig, double[] y, boolean plotIt) 
		throws IOException
	{
		// Write the signal data to file, too.
		PrintWriter wsd = new PrintWriter( new FileWriter("signal.dat"), true );

		double t = 0.0; double hs = W / noPtsSig;
		
		for (int i = 0; i < noPtsSig; i++, t+=hs)
		{
			double t1 = W/6., t2 = 4.*W/6.;       // signal boundaries: iT-t1-t2-fT

			if ( t>= iT && t<=t1 )
				y[i] = Math.sin(2*PI*t);
			else if ( t>=t1 && t<=t2 )
				y[i] = 5.*Math.sin(2*PI*t) + 10.*Math.sin(4*PI*t);
			else if ( t>=t2 && t<=fT )
				y[i] = 2.5*Math.sin(2*PI*t) + 6.*Math.sin(4*PI*t) + 10.*Math.sin(6*PI*t);
			else {
				System.err.println("\n\tIn signal(...) : t out of range.\n");
				System.exit(1);
			}
			//y[i] = Math.sin(2*Math.PI*t); //y[i] = Math.sin(3*t);
			//y[i] = Math.sin(t) + Math.sin(3*t) + Math.sin(5*t);
			// if ( t>=0 && t < W/2. ) y[i] = Math.sin(PI*t);
			// else y[i] = 0;
			wsd.println(t + " " + y[i]);
		}
		wsd.close();

		if (plotIt) {                         // show it, if the plot is asked for
			Plot ptSig = new Plot();
			for (int i = 0; i < noPtsSig; i++)
				ptSig.addPoint(0, i*h, y[i], true);
			ptSig.setXLabel("time"); ptSig.setYLabel("signal");
			ptSig.setTitle("Signal: 1 or 2 or 3 sin's (different over 3 intervals)");
			PlotApplication showSig = new PlotApplication(ptSig);
		}
	}

	// Set statics, overloaded (and recalculate other variables dependent on them).
	/** Set class variables: number of points for the transform. */
	public static void setParameters(int ptsTransf)
	{ 
		N = ptsTransf; h = W/N; noPtsSig = N;
	}
	/** Set class variables: number of points for time translation, scale. */
	public static void setParameters(int ptsTau, int ptsS)
	{
		noTau = ptsTau; noS = ptsS;
		dTau = W/noTau; dS = Math.pow(W/iS, 1./noS);
	}
	/** Set class variables: number of points for transform, time translation, scale. */
	public static void setParameters(int ptsTransf, int ptsTau, int ptsS)
	{
		N = ptsTransf; h = W/N; noPtsSig = N;
		noTau = ptsTau; noS = ptsS;
		dTau = W/noTau; dS = Math.pow(W/iS, 1./noS);
	}
	
	/** Comparison: set the scale for the inverse transform to be equal to that
	 *  for the signal (via their max values); then plot both on the same graph.
	 */
	public static void plotCompare(double[] sig, double[] inv) throws IOException
	{
		double maxSig = 0.001, maxInv = 0.001;
		for (int i = 0; i < sig.length; i++)              // find max for the signal
			if ( sig[i] > maxSig || sig[i] < -1*maxSig )
				maxSig = sig[i];
		for (int i = 0; i < inv.length; i++)              // find max inverse transf.
			if ( inv[i] > maxInv || inv[i] < -1*maxInv )
				maxInv = inv[i];
		
		for (int i = 0; i < inv.length; i++)              // scale inverse to signal
			inv[i] = (inv[i] / maxInv) * maxSig;

		// Plot them; and write data for normalized reconstructed signal to file.
		String recSigNorm = "recSigNorm.dat";
		PrintWriter wNRSd = new PrintWriter( new FileWriter( recSigNorm ), true);
		Plot plComp = new Plot();
		double t = 0.0;
		for (int i = 0; i < sig.length && i < inv.length; i++, t+=h) {
			plComp.addPoint(0, t, sig[i], true);
			plComp.addPoint(1, t, inv[i], true);
			wNRSd.println(t + " " + inv[i]);
		}
		wNRSd.close();
		
		plComp.setXLabel("time"); plComp.setYLabel("signal, inverse transform");
		plComp.setTitle("Signal, and its reconstruction (normalized)");
		plComp.addLegend(0, "Signal"); plComp.addLegend(1, "Reconstructed");
		PlotApplication shComp = new PlotApplication(plComp);
	}
			
	/** Plot mother wavelets, for given tau and s. (As it stands, plots Morlet.) */
	public static void plotWavelets(double tau, double s) 
	{ 
		// To plot others: change morlet() call to haar(), mexhat(); adjust title.
		Plot ptMorlet = new Plot();
		for (double t = -W/2.; t < W/2.; t+=h) {
			ptMorlet.addPoint(0, t, morlet(t, s, tau), true);
		}
		String morletTitle = "Morlet wavelet (initial: tau="+iTau+", s="+iS+")";
		ptMorlet.setXLabel("time"); ptMorlet.setYLabel("Psi"); 
		ptMorlet.setXRange(-10*iS , 10*iS);               // show conveniently
		ptMorlet.setTitle( morletTitle );

		PlotApplication shMorlet = new PlotApplication( ptMorlet );
	}

} // end of class Wavelets
