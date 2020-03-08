/*								 
	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
	 by RH Landau, MJ Paez, and CC BORDEIANU 
	 Copyright Princeton University Press, Princeton, 2007.
	 Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
	 MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
	 Support by National Science Foundation															 
*/
//  Solution of heat equation using  Crank-Nicolson method and Jama library                                        						       
                                                                                                                                             
import java.io.*;
import Jama.*;
public class HeatCNJama
	{
public static void main(String[] argv)throws IOException,
FileNotFoundException
{
	PrintWriter w = 
        new PrintWriter(new FileOutputStream("heat1.dat"), true);
    	// save data in heat1.dat 
    int n=100, m=100;//number of grid points 
    double t[][]=new double [n+1][m+1]; 
    double A1[][]=new double[n+1][m+1];
    double B1[][]=new double[n+1][1];
    int i, j;                // loop counters 
    double thc=0.12;// constants 
    double sph=0.113;
    double rho =7.8;
    double ct=thc/(sph*rho);        
    double r;     
    double u[]=new double[n+1];//temp
    double h,k;                
    h=1.0;//space step
    k=180.0;//time step
    r  = ct * k / ( h * h );
   //initialize
    for ( i = 0; i <= n; i++ ) {
    for ( j = 0; j <= m; j++ ) {
    
    t[i][j] = 0.0;
}
}
// boundary conditions 
     for ( j = 0; j <= m; j++ ) {

           t[0][j] = 0.0;
           t[n][j] = 0.0;
}
//initial conditions
    for ( i = 0; i <= n  ; i++ ) 
    t[i][0] = 100.0;
    
    for ( i = 0; i <= n; i++ ){
    	for(j=0;j<=m;j++){ 
    A1[i][j] = 0.0;
   if(i==j) A1[i][j]=2.0+(2.0/r);
   if(i==j+1) A1[i][j]=-1.0;
   if(j==i+1) A1[i][j]=-1.0;

}
}
Matrix a1 = new Matrix(A1); 
  
B1[0][0]=0.0;
B1[n][0]=0.0;

    for ( j = 1; j <= m; j++ )
    {
        for ( i = 1; i <= n-1 ; i++ ) 
        {
         
        B1[i][0] = t[i-1][j-1] + t[i+1][j-1] + ((2.0 / r)  -2.0) * t[i][j-1];
      
        }
        
       Matrix b1 = new Matrix(B1); 
       
       // solve the tridiagonal system.
       Matrix sol = a1.solve(b1);
       
       for(i=0;i<n-1;i++){ 
  
      u[i]=sol.get(i,0);//Jama get solution
      
      t[i][j] = u[i];
 }       
}      
// write data gnuplot 3D format 
     
for (j=0;j<=m-1;j++)
       { 
   if(j%5==0) {
      for ( i = 0; i <=n-1; i++ )
       { 
          w.println(""+t[i][j]+"");   
        }
 w.println();// empty line for gnuplot 
              }
  }
 System.out.println("data stored in heat1.dat");   
  
} 

}