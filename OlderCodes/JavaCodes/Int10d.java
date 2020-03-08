/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
//  Int_10d.java: 10D Monte-Carlo integration 
import java.io.*;
import java.util.*;
public class Int10d
{static int max =  65536, m =  32;         
 public static void main(String[] argv) throws IOException,
            FileNotFoundException
 { int i, j, k;
   double n = 1.0, x = 0, sum = 0, sum1 = 0;
   double y[] = new double[33];
   long seed = 899432;          //Initialize 48 bit generator
   Random r  = new Random(seed); 
   PrintWriter w =
     new PrintWriter(new FileOutputStream("Int_10D.dat"), true);
   for (k = 1; k <= m; k++)
   { for (i = 1; i <= max; i++)
    { x = 0;                  /* reset x */
      for (j=1; j<=10; j++)
      x += r.nextDouble();   /* sum of 10 x values */
      y[k] += x*x;    }
      w.println("int["+k+"] = "+y[k]/i); }
    System.out.println("data saved in Int_10D.dat");
   for (k = 1; k <= m; k++) sum += y[k];
   w.println("Average = " + sum/(m*max));
}}
