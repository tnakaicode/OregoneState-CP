/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Trap.java trapezoid-rule integration of parabola 

public class Trap  { 
  public static final double A = 0., B =3.;                     // Constant endpoints
  public static final int N = 100;                        // N points (not intervals)
    
  public static void main(String[] args) {                     // Main does summation
    double sum, h, t, w; 
    int i; 
    h = (B - A)/(N - 1);                                            // Initialization
    sum = 0.; 
    for ( i=1 ;  i <= N;  i=i + 1)  {                                    // Trap rule
      t = A + (i-1) * h; 
      if ( i==1 || i==N ) w = h/2.; else w = h;                         // End wt=h/2
      sum = sum + w  * t * t;      
    }
    System.out.println(sum); 
  } 
}                                                        // OUTPUT  9.000459136822773