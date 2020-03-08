/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/
/*****************************************************************
Newton_Jama.java
with torques
Solve a system of n  nonlinear equations  using Newton-Raphson 
method and JAMA Matrix library (http://math.nist.gov/javanumerics/jama).
Jama directory (containing Jama library) /Jamamust be in  same directory or 
modify CLASSPATH variable to include  JAMA directory.
This program uses  Matrix class, i.e. Matrix as object
Numerical, central difference derivative
*****************************************************************/
import Jama.*;

public class Newton_Jama
{
public static void main(String[] argv)
{
int n = 9; int it, i;
double deriv[][] = new double[n][n], dx[] = new double[n];
double f[] = new double[n], errXi, errX, errF;
double eps = 1e-6;               //precision for convergence
//guess values for unknowns (all 0.5 also worked)
double x[] = {0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1., 1., 1.};

for ( it=1; it <= 100; it++)    // iteration loop, break when converge
    {
  F(x,f);            //compute F vector = the n equations, - the RHS
        dFi_dXj(x, deriv, n);        // compute array of derivatives dFi/dXj
                                //A = LHS matrix using Jama
Matrix A = new Matrix(deriv);                     
System.out.print("dFi_dXj for iteration " + it);                    
A.print(7,4);                   // printout of dFi_dXj via Jama
                                // Update solution
Matrix B = new Matrix(n,1);
for ( i=0; i <= n-1; i++) B.set(i, 0, -f[i]);               //RHS of system
    System.out.print("RHS vector [f(Xguess)]  for iteration " + it);           //rhs vector 
    B.print(17,4); 
                        //solution vector sol = dx corrections via Jama solve
    Matrix sol = A.solve(B);       
    System.out.print("dx corrections for iteration "  + it);
    sol.print(17,4);
    System.out.println (" x + dx for iteration " + it);
    for ( i=0; i <= n-1; i++) {
        dx[i] = sol.get(i,0);
  x[i]  = x[i] + dx[i];         //increment solution
    System.out.println("x["+i+"] =  "+x[i]);}           //printout final solution
    System.out.println();
    errX = errF = errXi = 0.0;                      //set errors to 0
    //          Check for Convergence; find max err, then break if < eps
        for ( i = 0; i <= n-1; i++){
        if ( x[i] != 0.) errXi = Math.abs(dx[i]/x[i]);
           else errXi = Math.abs(dx[i]);
        if ( errXi > errX) errX = errXi;                       // solution error
        if ( Math.abs(f[i]) > errF ) errF = Math.abs(f[i]);    // functions error
    }
     if ((errX <= eps) && (errF <= eps)) break;  // check for convergence
}
}
// The nonlinear system equations
// You can enter your own system here
public static void F(double x[], double f[])
{// n-number of equations
    f[0] = 90*x[3] + 80*x[4] - 8*x[8]*x[2];
    f[1] = 120*x[5] + 30*x[4] - 8*x[6]*x[0];
    f[2] = x[6]*x[0] - x[7]*x[1] - 10.0;
    f[3] = x[6]*x[3] - x[7]*x[4];
    f[4] = x[7]*x[1] + x[8]*x[2] - 20.0;
    f[5] = x[7]*x[4] - x[8]*x[5];
    f[6] = Math.pow(x[0],2) + Math.pow(x[3],2) - 1.0;
    f[7] = Math.pow(x[1],2) + Math.pow(x[4],2) - 1.0;
    f[8] = Math.pow(x[2],2) + Math.pow(x[5],2) - 1.0;
}

public static void dFi_dXj(double x[], double deriv[][], int n)
//compute derivatives of n eqns  using central difference
{ 
int i,j; double temp; double h = 1e-4;
double f[] = new double[n];
h  =   1e-4;                 //differentiation step

//          Loop for x + h/2
for (j = 0; j <=  n-1; j++)
{
    temp = x[j];
    x[j] += h/2.;
    F(x, f);             //store  function at x[j] = x[j]+h/2.
    for (i = 0; i <=  n-1; i++) { deriv[i][j] = f[i];}
    x[j] = temp;        //reset x[j]
}

//          Loop for x - h/2
for (j = 0; j <=  n-1; j++) 
    {
  temp = x[j];
  x[j] = x[j]-h/2.;
  F(x, f);                    //store  function at x[j] = x[j]-h/2.
  for (i = 0; i <=  n-1; i++) { deriv[i][j] = (deriv[i][j]-f[i])/h;}
        x[j] = temp;            //reset x[j]
    }
}
}
