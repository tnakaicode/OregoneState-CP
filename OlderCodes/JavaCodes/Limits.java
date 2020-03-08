/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Limits.java: Determines machine precision 

public class Limits  { 

  public static void main(String[] args)  {
    final int N = 60;        
    int i;                                                 
    double eps = 1., onePlusEps;                             
    for ( i = 0;  i < N;  i=i + 1)  { 
      eps = eps/2.; 
      onePlusEps = 1. + eps;  
      System.out.println("onePlusEps = " +onePlusEps+", eps = "+eps); 
}  }  } 
