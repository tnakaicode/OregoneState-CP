/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
	 by RH Landau, MJ Paez, and CC Bordeianu 
	 Copyright Princeton University Press, Princeton, 2008.
	 Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
	 MJ Paez, Univ Antioquia, 2008; and CC Bordeianu, Univ Bucharest, 2008.
	 Support by National Science Foundation.
	 Java Program written by Zlatko Dimcovic
*/
/**
 * This is a simple demo of how to read in arguments the program is invoked with, and do 
 * some simple parsing.	 Please note that we should really check whether the types of the
 * supplied arguments match how we use them; a convenient way would be to "try" them for 
 * exceptions (that the system raises when we read a character string (text) into an int, 
 * for example), then "catch" exceptions and print an informative message. (The "try" and 
 * "catch" are the key-words used when working with exceptions.) As we are not doing this, 
 * if one supplies, for example, a non-numeric string where a number is expected, this 
 * program will crash (the compiler will tell us the exact exception that caused this).
 */

/** Parse command line arguments. They are stored in an array of Strings, which is passed
 * to main() (with a typical name "argv"); everything typed on the command line after the
 * class's name is broken by white-space into words, each becoming a String in this array.
 * (This can be affected by using quotes.) Then in the main() method we go through this 
 * array and read its elements into our variables. Since argv[] elements are Strings, we 
 * may have to convert them into data types that our variables require; for our variables 
 * declared to be Strings, like file names, no conversion is needed. C Methods  
 * "Double" and "Integer" are used to convert String type to double and int types.

 Compile: g++ CmdLineArgsDemo
 run like: ./a.out 25 56.7 
the output will be:

 filename= ./a.out Arguments supplied: intParam (1st) =  25 ,doubleParam (2nd)= 56.700000
 filename= ./a.out Arguments supplied: intParam (1st) =  25 ,doubleParam (2nd)= 56.700000
Name of executable file: ./a.out

 */
#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>
main(int argc, char *argv[]) {
  int intParam;                 // First argv
  double doubleParam;         // second argv
  char *filename;     // Will form the rest (or read it in).
/* We accept 2 or 3 arguments on the command line; and we want them of the following
 * types, and in the following order: int double string (optional). So an accepted
 * invokation of this program would be: ./a.out anInt aDouble [aString].
 * Below we check whether the "args" array have these numbers of arguments; note that
 * we do not check the types (what one should do). We also use the arguments to form
 * the filename -- unless this is overridden by the actual filename, which can also be
 * supplied (optionally) as the last argument.  */

 if (argc == 2 || argc == 3) { // We demand 2 or 3 arguments;
 // and we want to set these two
 intParam= atoi(argv[1]) ;	//convert string to integer this is the secon argument
 doubleParam = atof(argv[2]) ;  //converts string to double
 if ( argc== 3 )     // If the 3rd argument is given,	
   filename = argv[0];       //this is the first argument
}
 else {// no else -- exit (with a message)
   printf("\n\tUsage: java CmdLineArgsDemo intParam doubleParam ");
   printf("\tThe 1st argument must be an int, the 2nd a double (or an int)\n\tthe (optional) 3rd a string.\n");	
// "\n" not portable; use println();
   exit(1);
  } 
  printf(" filename= %s Arguments supplied: intParam (1st) =  %d ,doubleParam (2nd)= %lf\n",argv[0], intParam, doubleParam);
  if ( argc== 3 ) 
  printf("Name of executable file: %s \n", filename);
 
}
