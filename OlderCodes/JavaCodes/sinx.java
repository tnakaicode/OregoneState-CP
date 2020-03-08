/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/
/* Computes sin(x) using Taylor expansion. The series stops when
 * |term/sum| <10-8. If x>2*pi  then x= x mod(2pi).
 * Prints sin(x) using internal function sin, sin(x) using exanpansion
 * the last term added to the sum and the iterations used when the series stops
 * Change x at will.
 */                      
import java.io.*;
public class sinx {
  public static void main(String[] argv) 
    throws IOException, FileNotFoundException{
   // PrintWriter q = new PrintWriter(
    //   new FileOutputStream("Walk.dat"),true);
    double sum, x,term,i,eps,quo,res,twopi;
    int n;
    n=10;
    x=100;
    twopi=2*Math.PI;
    if(x>2*Math.PI){                            //case x >2*PI
       quo=(int)(x/(twopi));
       res=x-quo*twopi;                       //residue
       x=res;                                 //make x the residue
    }
    sum=x;
    term=x;
    i=0;
    //for(i=1;i<n;i++){
    do{
     i=i+1;
     term=-term*x*x/((2*i+1)*2*i);
     sum=sum+term;
   
    }while(Math.abs(term/sum)>10e-8);
    System.out.println("sen("+ x +")=" + Math.sin(x) + " sum "+ sum);
    System.out.println("term "+ term +" iter " +i);
   }//main
 }//class 