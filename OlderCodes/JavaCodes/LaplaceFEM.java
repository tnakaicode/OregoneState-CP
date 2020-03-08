/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
/*  LaplaceFEM3.java, solution of 1D Poisson equation  
  using Finite Element Method with Galerkin approximation  */   
import Jama.*; 
import java.io.*; 

public class LaplaceFEM  { 
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w =  new PrintWriter(new FileOutputStream("fem3.dat"), true); 
    PrintWriter q =  new PrintWriter(new FileOutputStream("fem3t.dat"), true);
    PrintWriter t =  new PrintWriter(new FileOutputStream("fem3e.dat"), true); 
    int i, j; int N = 11; 
    double u[] = new double[N], A[][] = new double[N][N], b[][] = new double[N][1];  
    double x2[] =new double[21], u_fem[] = new double[21], u_exact[] =new double[21];
    double error[] = new double[21], x[] = new double[N], h = 1./(N-1);   
    for ( i=0; i <= N-1; i++ )  { 
      x[i] = i*h; 
      System.out.println("" + x[i] + ""); 
    }
    for ( i=0; i <= N-1; i++ ) {                                        // Initialize
      b[i][0] = 0. ; 
      for ( j=0; j <= N-1; j++ )  A[i][j] = 0. ;   
    }
    for ( i=1; i <= N-1; i++ )  {
      A[i-1][i-1] = A[i-1][i-1] + 1./h; 
      A[i-1][i]  = A[i-1][i] - 1./h; 
      A[i][i-1]  = A[i-1][i]; 
      A[i][i]    = A[i][i] + 1./h; 
      b[i-1][0]  = b[i-1][0] + int2(x[i-1], x[i]); 
      b[i][0]    = b[i][0] + int1(x[i-1], x[i]); 
    }
  
    for ( i=1; i <= N-1; i++ ) {                           // Dirichlet BC @ left end
      b[i][0] = b[i][0]-0.*A[i][0]; 
      A[i][0] = 0. ; 
      A[0][i] = 0. ;  
    }
    A[0][0] = 1. ; 
    b[0][0] = 0. ; 
    for ( i=1; i <= N-1; i++ )  {                         // Dirichlet bc @ right end
      b[i][0] = b[i][0]-1.*A[i][N-1]; 
      A[i][N-1] = 0. ; 
      A[N-1][i] = 0. ;  
    }
    A[N-1][N-1] = 1. ; 
    b[N-1][0] = 1. ; 
    Matrix A1 = new Matrix(A);                                  // Jama matrix object
    Matrix b1 = new Matrix(b); 
    A1.print (16, 14);                                               // Jama print A1
    b1.print (16, 14);                                               // Jama print b1
    Matrix sol = A1.solve(b1);                           // Jama solves linear system
    sol.print (16, 14);                                        // Jama print solution
    for ( i=0; i <= N-1; i++ ) u[i] += sol.get(i, 0);                 // Get solution
    for ( i=0; i <= 20; i++ )  x2[i]=0.05*i;   
    for ( i=0; i <= x2.length-1; i++ ) {
      u_fem[i] = numerical(x, u, x2[i]); 
      u_exact[i] = exact(x2[i]); 
      q.println(" " + 0.05*i + " " + u_exact[i] + " "); 
      w.println(" " + 0.05*i + " " + u_fem[i] + " "); 
      error[i] = u_fem[i]-u_exact[i];                                 // Global error
      t.println(" " + 0.05*i + " " + error[i] + " "); 
    } 
}

  public static double int1 (double min, double max)  {                    // Simpson
    int n, no = 1000;                 
    double interval, sum = 0., x; 
    interval = ((max -min) /(no-1)); 
    for ( n=2;  n < no;  n += 2)  {                                // Loop odd points
      x = interval * (n-1); 
      sum += 4 * f(x)*lin1(x, min, max);  
    }
    for ( n=3;  n < no;  n += 2) {                                // Loop even points
      x = interval * (n-1); 
      sum += 2 * f(x)*lin1(x, min, max);  
    }   
    sum += f(min)*lin1(min, min, max) + f(max)*lin1(max, min, max);                
    sum *= interval/6.;   
    return (sum); 
  }  
  
  public static double int2 (double min, double max)  {  
    int n, no = 1000;                 
    double interval, sum = 0., x; 
    interval = ((max -min) /(no-1)); 
    for ( n = 2;  n < no;  n += 2)    {                            // Loop odd points
      x = interval * (n-1); 
      sum += 4 * f(x)*lin2(x, min, max); 
    }
    for ( n=3;  n < no;  n += 2)  {                               // Loop even points
      x = interval * (n-1); 
      sum += 2 * f(x)*lin2(x, min, max); 
    }   
    sum += f(min)*lin2(min, min, max) + f(max)*lin2(max, min, max); 
    sum *= interval/6.;  
    return (sum); 
  }  
  
  public static double int1f(double min, double max)  {                // 2nd Simpson
    double xm, sum; 
    xm = (min + max)*0.5; 
    sum = (max-min)*(f(min)*lin1(min, min, max) 
          + 4*f(xm)*lin1(xm, min, max) + f(max)*lin1(max, min, max) )/6;
    return sum; 
  } 
  
  public static double int2f(double min, double max)  { 
    double xm, sum; 
    xm = (min + max)*0.5; 
    sum = (max-min)*(f(min)*lin2(min, min, max) 
   + 4*f(xm)*lin2(xm, min, max) + f(max)*lin2(max, min, max))/6.; 
  return sum; 
 }

  public static double lin1(double x, double x1, double x2)              // Hat funcs
       { return  (x-x1)/(x2-x1); }
       
  public static double lin2(double x, double x1, double x2)
       { return (x2-x)/(x2-x1); }
       
  public static double f(double x)                             // RHS of the equation
   { return 1. ; }
   
  public static double numerical(double x[], double u[], double xp)   { 
    int i, N = 11;                                  // interpolate numerical solution
    double y; y = 0. ; 
    for ( i=0; i <= N-2; i++ )  {
      if ( xp >=x[i] && xp <= x[i + 1] )  y = lin2(xp, x[i], 
                       x[i+1])*u[i] + lin1(xp, x[i], x[i + 1])*u[i + 1];
    }
  return y; 
  }
  
  public static double exact(double x){                          // Analytic solution
    double u;    
    u = -x*(x-3.)/2. ; 
   return u; 
  }
}
