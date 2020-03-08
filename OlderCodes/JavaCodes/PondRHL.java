/* 
************************************************************************
*  pond.java: *Monte-Carlo integration to determine pi (stone throwing)* 
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*                                                                      *
************************************************************************
*/
import java.io.*;
import java.util.*;
                 
 public class PondRHL

  {             
      
static int  max= 1000;                     /* number of stones to be thrown */
static int seed= 68111 ;                  /* seed for number generator */

public static void main(String[] argv) throws IOException,
FileNotFoundException
{
   int i, pi=0;
   double x, y, area;
   
   PrintWriter w = 
        new PrintWriter(new FileOutputStream("Pond.dat"), true);/* save data in pond.dat */
   
   Random r=new Random(seed);                    /* seed the number generator */
   

   for (i=1; i<=max; i++)
   {
      x = r.nextDouble()*2-1;             /* creates floats between */
      y = r.nextDouble()*2-1;             /* 1 and -1 */
      if ((x*x + y*y)<1) pi++;       /* stone hit the pond */
      area=4*(double)pi/i;           /* calculate area */

      w.println( "i= "+ i+" area= "+ area+" pi= "+Math.PI);
      
   }
   System.out.println("data stored in Pond.dat");
   
}
} 