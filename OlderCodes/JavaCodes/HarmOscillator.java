//-----------------------------------
// Last modified: 2006 Nov 05 (19:23)
//-----------------------------------

/* A note on documentation.	 Java comes with the tool 'javadoc,' that processes comments 
 * within a program that start with /** (2 asterisks).	Within them one may use some HTML
 * formatting (the first comment below does this, as an example). Run it on this file, as 
 * 'javadoc -d dirName file' (-d dirName is there only so that the files generated go into 
 * a separate directory). Then open the generated file HarmOscillator.html with a browser.
 * This is how java documentation, including the official pages on sun's site, is made.
 */

import java.io.*;
import ptolemy.plot.*;

/**
 * This is a simple demonstration of how one can parse command-line arguments,
 * and of a few related chores: 
 * <br /> &nbsp;&ndash;
 * <em>&ldquo;try&rdquo; for &ldquo;exceptions&rdquo; typical for this 
 * ({@code NumberFormatException}), and &ldquo;catch&rdquo; them 
 * (and do something simple)</em>
 * <br /> &nbsp;&ndash;
 * <em>File names created with values of variables in them, and plots labeled 
 * and exported with these, during run-time</em>
 * <br /> &nbsp;&ndash; 
 * <em>Setting static (class variables) to command-line arguments; and a few other 
 * bits and pieces</em>
 * <br />(We also illustrate a few simple uses of the 
 * <a href="http://java.sun.com/j2se/javadoc/">javadoc</a> tool; the simple formatting 
 * above is for this purpose.) <br />
 * For information on java see, for example, Sun's java pages:
 * <a href="http://java.sun.com/j2se/1.5.0/docs/index.html">JDK 5.0 Documentation</a>,
 * <a href="http://java.sun.com/docs/books/tutorial/index.html">various tutorials</a>
 * (with a 
 * <a href="http://java.sun.com/docs/books/tutorial/reallybigindex.html">&ldquo;Big 
 * Index&rdquo;</a>). <br />Also, becoming able to use, and getting used to, 
 * <a href="http://java.sun.com/j2se/1.5.0/docs/api/index.html">API Specification</a>
 * &ndash; for any language &ndash; is usually a very good idea.
 */

public class HarmOscillator {

	static final double PI = Math.PI;			 // A constant: "final" cannot be changed.
																				 // Parameters used throughout; defaults:
	static int		N		= 100;							 // number of iterations (sampling points)
	static double w		= 1.0;							 // frequency
	static double dC	= 0.1;							 // damping coefficient 
	static double amp = 5.0;							 // amplitude
	
	public static void main(String[] args) throws IOException, FileNotFoundException 
	{ 
	
		String filename = "oscillator";								 // Base for names (formed below):
		String exportPlotTo = "oscPlotExport";				 // for data and exported plot.

		/* Note the use of "scoping" below: we may use exact same variable names as we
		 * have defined above, since N,w,amp below are local in scope to the block in 
		 * which they are declared; and they "shadow" those set above, within this block.
		 * (Variables exist only within the innermost block of code they are declared in.)
		 * This is mostly for demonstration though; be careful with it, errors do arise.
		 */
		if ( args.length == 3 || args.length == 4 ) 
		{
			int N = 0;	// Can't declare inside the "try" block: it wouldn't exist outside!
			try {																					// Use "exceptions": java "throws"
				N = Integer.parseInt( args[0] );						// them, when wrong things happen
			}																							// (eg: text read into a number),
			catch (NumberFormatException nfe) {						// but one has to "try" and "catch"
				System.err.println("The first argument has to be an integer.");
				System.exit(1);
			}
			// For brevity, we don't check now -- try running the program with bad arguments.
			double w	 = Double.parseDouble( args[1] ),		// Declare these new variables only
						 dC	 = Double.parseDouble( args[2] );		// for convenience, for passing to:
			params( N, w, dC );														// this method sets static's
			filename = filename + "_N" + N + "_w" + w + ".dat";
			exportPlotTo += "_N" + N + "_w" + w + ".eps"; // append to basenames
			if ( args.length == 4 )												// enter filename on cmdline?
				filename = args[3];
		}
		else { // exit with a message; print to System.err, so it is not redirected
			System.err.println(
				"\n\tUsage: java harmOscillator numIterations freq dampCoeff [filename.dat]\n");
			System.err.println(
				"\t( A reasonable invokation: java HarmOscillator 100 5 0.2 )\n");
			System.exit(1);																// note: "\n" not portable
		}

		PrintWriter fw = new PrintWriter( new FileWriter(filename), true );
		Plot pt = new Plot();

		double a = 0, b = 10, h = (b-a)/N;							// limits; step size
		double t = a; double x1 = amp; double x2 = amp;

		/* 'do-while' loop: normally used to make sure code in the body executes at least
		 * once, before the while test. ('while' loop may never execute, if the condition 
		 * fails to start with.)	Here we simply want to have t=0 point too, before +=h.
		 */
		do {
			x1 = f1(t);
			x2 = f2(t);
			pt.addPoint(0, t, x1, true);
			pt.addPoint(1, t, x2, true);
			fw.println(t + " " + x1 + " " + x2);
		} 
		while ( (t += h) < b );							 // increment t by h, then test this against b

		pt.setXLabel("time"); pt.setYLabel("damped sin; sin");
		pt.setMarksStyle("dots", 0); pt.setMarksStyle("points", 1);
		pt.addLegend(0, "exp(-g*t)*sin(t)"); pt.addLegend(1, "sin(t)");
		// Prepare title; show only one decimal place (correctly rounded)
		String ptTitle = roundToDecPlaces(w*N*h/(2*PI), 1) + " cycles; freq = " 
			+ roundToDecPlaces(w, 2) + ", damp: g=" + dC + "	(step : " + h + ")"; 
		pt.setTitle( ptTitle );

		PlotApplication showPt = new PlotApplication(pt); // now draw plot on screen
		pt.export( new FileOutputStream(exportPlotTo) );	// export plot (to eps format)

	} // end of main

	/** Set class variables: number of iterations, frequency, damping coefficient. */
	public static void params (int noCyc, double freq, double dampCoeff) 
	{
		N = noCyc; w = freq; dC = dampCoeff;
	}
	/** Set class variables, overloaded: also set amplitude. */
	public static void params (int noCyc, double freq, double dampCoeff, 
			double amplitude)
	{
		N = noCyc; w = freq; dC = dampCoeff; amp = amplitude;
	}

	/** Round {@code numToRound} to the {@code precision} number of decimal places.
	 * It makes use of the
	 * <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/lang/Math.html#round(double)">
	 * java.Math.round(double)</a> method.
	 * @see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/text/DecimalFormat.html#format(double)">
	 * java.text.DecimalFormat.format(double)</a>
	 * @see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/util/Formatter.html">
	 * java.util.Formatter</a>
	 * @see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/text/package-summary.html">
	 * java.text.*</a>
	 * @see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/lang/String.html#format">
	 * String.format(...)</a>
	 * @see <a href="http://java.sun.com/j2se/1.5.0/docs/api/java/math/BigDecimal.html">
	 * java.math.BigDecimal</a>
	 */
	/*
	* There are more systemic ways; for example, one would be to use formatting from 
	* java.text.DecimalFormat class (java.text.DecimalFormat then has to be import-ed):
	* DecimalFormat df = new DecimalFormat("#0.0"); df.format(w); -- and then whenever 
	* you want such format to be applied you use df.format(var); this returns a string,
	* and it is meant for displaying values conveniently. One can also do it in one go: 
	* (new DecimalFormat("#0.0")).format(w). Or we can use casting, with a possible slight 
	* loss of precision: ((int) (10*w))/10.0. Also, one can use C-style formatting with 
	* System.out.printf(...). And then there is a number of yet other ways; see links.
	*
	* Note that our method rounds to a (decimal representation of) an int when invoked 
	* with precision<=0, while it should really raise an error/exception. 
	*/
	public static double roundToDecPlaces(double numToRound, int precision)
	{
		double factor = 1.;
		for (int i = 0; i < precision; i++)
			factor *= 10.0 ;							// or use factor=Math.pow(10.0, precision)
		return ( precision > 0 )
			? (Math.round( factor * numToRound )) / factor
			: Math.round( numToRound );		// for prec<=0 should really report error
	}

	/** Computes position for a given time for a damped harmonic oscillator.
	 *	(It should check for errors during the computation, and throw exceptions
	 *	or return small integer indicating success/failure; but it does not.)
	 *	@param t time at which to calculate position
	 *	@return the position at this time.
	 */
	public static double f1(double t)
	{
		 return amp * Math.exp(-dC*t) * Math.sin(w*t);
	}

	/** Computes position for a given time for a harmonic oscillator. */
	public static double f2(double t)
	{
		 return amp * Math.sin(w*t);
	}
}
