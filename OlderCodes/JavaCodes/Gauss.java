/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// gauss.java Gaussian quadrature points and weights
public class Gauss{
	
  public static void gauss(int npts, int job, double a, double b, 
	                                                          double x[], double w[]) {
      int m = 0, i = 0, j = 0; 
      double t = 0., t1 = 0., pp = 0., p1 = 0., p2 = 0., p3 = 0., xi;
      double eps = 3.E-14;                                       // Accuracy: ADJUST!
      m = (npts + 1)/2; 
      for ( i=1;  i <= m;  i++ )  { 
        t = Math.cos(Math.PI*((double)i-0.25)/((double)npts + 0.5));
        t1 = 1; 
        while((Math.abs(t-t1)) >= eps)  {
          p1 = 1. ;  p2 = 0. ; 
          for ( j=1;  j <= npts;  j++ )  {
            p3 = p2;   p2 = p1; 
            p1=((2.*(double)j-1)*t*p2-((double)j-1.)*p3)/((double)j); 
          }
          pp = npts*(t*p1-p2)/(t*t-1.); 
          t1 = t;        t = t1 - p1/pp;  
        }   
        x[i-1] = -t;   x[npts-i] = t; 
        w[i-1]   = 2./((1.-t*t)*pp*pp); 
        w[npts-i] = w[i-1];  
        System.out.println(" x[i-1]"+ x[i-1] +" w " + w[npts-i]);
      } 
      if (job==0)  {
        for ( i=0;  i < npts ;  i++ )  {
          x[i] = x[i]*(b-a)/2. + (b + a)/2.; 
          w[i] = w[i]*(b-a)/2.; 
        }
      }
      if (job==1)  {
        for ( i=0;  i < npts;  i++ )  {
          xi=x[i];
          x[i] = a*b*(1. + xi) / (b + a-(b-a)*xi); 
          w[i] = w[i]*2.*a*b*b/((b + a-(b-a)*xi)*(b+a-(b-a)*xi));
        }
      }
      if (job==2) {
        for ( i=0;  i < npts;  i++ )  {
           xi=x[i];
           x[i] = (b*xi+ b + a + a) / (1.-xi); 
          w[i] = w[i]*2.*(a + b)/((1.-xi)*(1.-xi));  
        }
      }
      return;  
  }
}