/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
	 by RH Landau, MJ Paez, and CC Bordeianu 
	 Copyright Princeton University Press, Princeton, 2008.
	 Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
	 MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
	 Support by National Science Foundation.
	 Program written by Zlatko Dimcovic
*/
/* Note: two comments below start with /** so that they are seen by 'javadoc': you can run 
 * 'javadoc cmdLineArgsDemo.java' and HTML documentation for this program will be created; 
 * open the file 'cmdLineArgsDemo.html' with your browser, and these comments will show. 
 * If you want the generated files to go to a separate directory (as opposed to being 
 * written right into this one), use: 'javadoc -d dirName cmdLineArgsDemo.java'. Opening 
 * the 'index.html' in a browser will show you the same pages but organized with 'frames.' 
 * (This is more suitable for large projects, that consist of many classes.) Also note:	 
 * there is quite a bit more to 'javadoc' (see documentation on it). This is, for example, 
 * how java API documentation (on the Sun's java site) is organized.
 */

import java.io.*;

/**
 * This is a simple demo of how to read in arguments the program is invoked with, and do 
 * some simple parsing.	 Please note that we should really check whether the types of the
 * supplied arguments match how we use them; a convenient way would be to "try" them for 
 * exceptions (that the system raises when we read a character string (text) into an int, 
 * for example), then "catch" exceptions and print an informative message. (The "try" and 
 * "catch" are the key-words used when working with exceptions.) As we are not doing this, 
 * if one supplies, for example, a non-numeric string where a number is expected, this 
 * program will crash (and Java will tell us the exact exception that caused this).
 */

public class CmdLineArgsDemo {

	/** Parse command line arguments. They are stored in an array of Strings, which is passed
	 * to main() (with a typical name "args"); everything typed on the command line after the
	 * class's name is broken by white-space into words, each becoming a String in this array.
	 * (This can be affected by using quotes.) Then in the main() method we go through this 
	 * array and read its elements into our variables. Since args[] elements are Strings, we 
	 * may have to convert them into data types that our variables require; for our variables 
	 * declared to be Strings, like file names, no conversion is needed. Methods from java
	 * classes "Double" and "Integer" are used to convert String type to double and int types.
	 */
	
	public static void main(String[] args) {

		int intParam = 0;															 // We may also choose some reasonable
		double doubleParam = 0.0;											 // defaults, so arguments are optional.
		String filename = "baseName";									 // Will form the rest (or read it in).
		
		/* We accept 2 or 3 arguments on the command line; and we want them of the following
		 * types, and in the following order: int double string (optional). So an accepted
		 * invokation of this program would be: java CmdLineArgsDemo anInt aDouble [aString].
		 * Below we check whether the "args" array have these numbers of arguments; note that
		 * we do not check the types (what one should do). We also use the arguments to form
		 * the filename -- unless this is overridden by the actual filename, which can also be
		 * supplied (optionally) as the last argument. Just in case, here is a little sketch:
		 * when we run the program:
		 *
		 * java CmdLineArgsDemo	 7			 5.32			name.dat
		 *											 ^			 ^^^^			^^^^^^^^
		 * the args array is:	 args[0]	args[1]		args[2]
		 *
		 * (The extra spaces above are only so we can show arguments lined up with args[].)
		 */

		if (args.length == 2 || args.length == 3) {			// We demand 2 or 3 arguments;
																										// and we want to set these two
			intParam		= Integer.parseInt	( args[0] );	// variables in both cases.
			doubleParam = Double.parseDouble( args[1] );

			if ( args.length == 3 )												// If the 3rd argument is given,
				filename = args[2];													// this is the desired filename.
			else																					// Or, append to "baseName" :
				filename += "_i" + intParam + "_d" + doubleParam + ".dat";
		}
		else {																					// no else -- exit (with a message)
			System.err.println("\n\tUsage: java CmdLineArgsDemo intParam doubleParam [filename]");
			System.err.println("\tThe 1st argument must be an int, the 2nd a double (or an int),"
				+ "\n\tthe (optional) 3rd a string.\n");		// "\n" not portable; use println();
			System.exit(1);
		} // System.err is used so that these do not get redirected accidentally.

		System.out.println("Arguments supplied: intParam (1st) = " + intParam
			+ ", doubleParam (2nd) = " + doubleParam);
		if ( args.length == 3 ) 
			System.out.println("And the optional string (text) was supplied, too: " + filename);
		else if (args.length == 2)
			System.out.println("Filename not given; add input values to the base: " + filename);
		else {
			System.err.println("\n\tERROR ! args.length should strictly be 2 or 3.\n");
			System.exit(1);
		}
	}
}
