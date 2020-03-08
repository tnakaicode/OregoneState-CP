/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// HeatCNTridiag.java: heat equation via Crank-Nicholson    
// Output in gnuplot 3D grid format
import java.io.*;

public class HeatCNTridiag { 
	
  public static void main(String[] argv)throws IOException, FileNotFoundException {
	  PrintWriter w =  new PrintWriter(new FileOutputStream("HeatCNTriD.dat"), true);
		int Max =51, i, j, n=50, m=50;  
		double Ta[] = new double[Max], Tb[] = new double[Max], Tc[] = new double[Max];
    double Td[] = new double[Max], a[]  = new double[Max], b[]  = new double[Max];
		double c[] = new double[Max], d[] =new double[Max], t[][]=new double [Max][Max];                             
		double width = 1.0, height = 0.1, ct = 1.0, k, r, h;          //  Rectangle W & H
    double x[] = new double[Max], Pi = 3.1415926535;
		   
    for ( i = 0; i < n; i++ ) t[i][0] = 0.0;                            // Initialize
		for ( i = 1; i < m; i++ ) t[0][i] = 0.0;
    h  = width / ( n - 1 );                      // Compute step sizes and constants
    k  = height / ( m - 1 );
    r  = ct * ct * k / ( h * h );
		for ( j = 1; j <= m; j++ ) { t[1][j] = 0.0; t[n][j] = 0.0; }               // BCs
    for ( i = 2; i <= n-1  ; i++ ) t[i][1] = Math.sin( Pi * h *i);             // ICs
    for ( i = 1; i <= n ; i++ )  Td[i] = 2. + 2./r;
		Td[1] = 1.; Td[n] = 1.;
    for ( i = 1; i <= n - 1 ; i++ ) {Ta[i] = -1.0;Tc[i] = -1.0;}      // Off diagonal
    Ta[n-1] = 0.0; Tc[1] = 0.0; Tb[1] = 0.0;Tb[n] = 0.0;
    for ( j = 2; j <= m; j++ )  {
      for (i = 2; i <= n-1; i++) Tb[i] = t[i-1][j-1]+t[i+1][j-1]+(2/r-2) * t[i][j-1];
      Tridiag(a,d,c,b,Ta,Td,Tc,Tb,x,n);                               // Solve system
      for ( i = 1; i <= n; i++ ) t[i][j] = x[i];
    }
    for (j=1;j<=m;j++) { 
			for ( i = 1; i <= n; i++ ) w.println(""+t[i][j]+"");   
      w.println();                                         // Empty line for gnuplot
    }
    System.out.println("data stored in HeatCNTridiag.dat");   
  } 

  public static void Tridiag(double a[],double d[],double c[],double b[],
                double Ta[],double Td[],double Tc[],double Tb[],double x[],int n) { 
    int i, Max = 51;
    double h[] = new double[Max], p[] = new double[Max];
    for (i = 1; i <= n; i++) {a[i] = Ta[i]; b[i] = Tb[i]; c[i] = Tc[i]; d[i] =Td[i];}
    h[1] = c[1]/d[1];        p[1] = b[1]/d[1];
    for ( i = 2; i <= n; i++ ) { 
			h[i]=c[i]/(d[i]-a[i]*h[i-1]);  p[i]=(b[i]-a[i]*p[i-1])/(d[i]-a[i]*h[i-1]); }
    x[n] = p[n];
    for ( i = n - 1; i >= 1; i-- ) x[i] = p[i] - h[i]*x[i+1];
  }
}
