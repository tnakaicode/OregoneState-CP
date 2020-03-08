/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
	 by RH Landau, MJ Paez, and CC Bordeianu 
	 Copyright Princeton University Press, Princeton, 2008.
	 Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
	 MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
	 Support by National Science Foundation															 
*/
/* CommandLineArgs.java: Accepts 2 or 3 arguments from command line, e.g.:
 		            java CommandLineArgs anInt aDouble [aString].
	[aString] is optional filename. See CmdLineArgsDemo on CD for full documentation
	Written by Zlatko Dimcovic */
	
public class CommandLineArgs {

	public static void main(String[] args) {
		int intParam = 0;																		           // Other values OK
		double doubleParam = 0.0;										           // Defaults, args optional
		String filename = "baseName";								            // Will form/read in rest
		if (args.length == 2 || args.length == 3) {			            // Demand 2 or 3 args
			intParam		= Integer.parseInt	( args[0] );	 
			doubleParam = Double.parseDouble( args[1] );
			if ( args.length == 3 )	filename = args[2];               // 3rd arg = filename
			else filename += "_i" + intParam + "_d" + doubleParam + ".dat";
		}
		else {															            // No else, exit with instruction
			System.err.println("\n\t Usage: java CmdLineArgs intParam doubleParam [file]");							                         //	"\n" not portable; use println()
			System.err.println("\t 1st arg must be int, 2nd double (or int),"
																 + "\n\t (optional) 3rd arg = string.\n");
			System.exit(1);
		}											           // System.err, used to avoid accidental redirect
		System.out.println("Input arguments: intParam (1st) = " + intParam
																+ ", doubleParam (2nd) = " + doubleParam);
		if (args.length == 3) System.out.println("String input: " +filename);
		else if (args.length == 2) System.out.println("No file, use" + filename);
		else {
			System.err.println("\n\tERROR ! args.length must be 2 or 3.\n");
			System.exit(1);
		}
} }
