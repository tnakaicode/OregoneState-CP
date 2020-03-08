/* 
************************************************************************
*  Fit.java:    Least square fit to decay spectrum                     *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
public class Fit
{ static int  data= 12;          /* number of data points */

public static void main(String[] argv)
{
   int i, j;
   double s, sx, sy, sxx, sxy, delta, inter, slope;
   double x[]=new double[data];
   double y[]=new double[data];
   double d[]=new double[data];
   

   for (i=0; i<data; i++) x[i]=i*10+5;    /* input data x */

   y[0]=32; y[1]=17;  y[2]=21; y[3]=7;            /* input data y */ 
   y[4]=8;  y[5]=6;   y[6]=5;  y[7]=2;          /* y[9] set to 0.1 */
   y[8]=2;  y[9]=0.1; y[10]=4; y[11]=1;         /* so that log exists */

   for (i=0; i<data; i++) d[i]=1.;     /* input data delta y */
                  /* estimate */
   
   for (i=0;i<data;i++) y[i]=Math.log(y[i]);    /* log(y[i]) for */ 
                                              /* exponential fit */     
   s=sx=sy=sxx=sxy=0;            /* reset sums */     

   for (i=0;i<data;i++)          /* calculating sums */
   {
      s   +=         1 / (d[i]*d[i]);
      sx  +=      x[i] / (d[i]*d[i]);
      sy  +=      y[i] / (d[i]*d[i]); 
      sxx += x[i]*x[i] / (d[i]*d[i]); 
      sxy += x[i]*y[i] / (d[i]*d[i]);
   } 

   delta = s*sxx-sx*sx;
   slope=  (s*sxy-sx*sy) / delta;        /* calculating all */
   inter=(sxx*sy-sx*sxy) / delta;        /* coefficients*/ 
   System.out.println("Fit intercept= "+inter+" , "+Math.sqrt(sxx/delta));
   System.out.println("Fit slope= "+slope+" , "+Math.sqrt(s/delta));
   System.out.println("Fit correlation= "+-sx/Math.sqrt(sxx*s));
   System.out.println("Fit Program Complete.");
}
}
