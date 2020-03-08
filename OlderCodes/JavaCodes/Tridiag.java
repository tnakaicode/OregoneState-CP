/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  Tridiag.java:  Solution of tri-diagonal linear system of equations

public static void Tridiag(double a[],double d[],double c[],double b[],
  double Ta[],double Td[],double Tc[],double Tb[],double x[],int n)   { 
  int i, Max = 51; 
  double h[] = new double[Max], p[] = new double[Max];            
  for (i = 1; i <= n; i++) {a[i] = Ta[i]; b[i] = Tb[i]; c[i] = Tc[i]; d[i] = Td[i];}
  h[1] = c[1]/d[1];    p[1] = b[1]/d[1]; 
  for ( i = 2;  i <= n;  i++ ) {
    h[i] = c[i]/(d[i]-a[i]*h[i-1]); 
    p[i] = (b[i]-a[i]*p[i-1])/(d[i]-a[i]*h[i-1]);       
    x[n] = p[n]; 
    for ( i = n - 1; i >= 1; i-- ) x[i] = p[i] - h[i]*x[i + 1]; 
  } 
}