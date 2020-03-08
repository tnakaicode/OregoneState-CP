/*
*****************************************************************************
* Diff.java: Differentiation with forward, central and  extrapolated        *
*            difference methods                                             *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
* 
* comment: results saved as x y1 y2 y3                                      *                                                                    
***************************************************************************** 
*/


import java.io.*;


public class Diff
 {     
public static double h= 1e-5;                       /* stepsize for all methods */
public static int xmax =7 ;                         /* range for calculation */
public static int xmin =0;
public static double xstep =0.01 ;                  /* stepsize in x  */

public static void main(String[] argv) throws IOException,
FileNotFoundException
{ 
   double dc, x;
   double result[]= new double[3];
                                        
   /* save data in diff.dat */
   
   PrintWriter w = 
        new PrintWriter(new FileOutputStream("diff.dat"), true);
   for (x=xmin; x<=xmax; x+=xstep)
   {
      result[0]= (f(x+h)-f(x))/h;              /* forward difference */
      result[1]=(f(x+h/2.)-f(x-h/2.))/h;/* central difference */
      result[2]=(8.*(f(x+h/4.)-f(x-h/4.))-(f(x+h/2.)-f(x-h/2.)))/(3.*h);
     /* extrapolated diff */
      w.println(""+ x+"  "+result[0]+"  "+result[1]+"  "+result[2]+"  ");
 
   }
   System.out.println("data stored in diff.dat");
   
}
/*---------------------end of main program----------------------------*/

/* the function we want to differentiate */
public static double f(double x)        
{
   return(Math.cos(x));
}
}
