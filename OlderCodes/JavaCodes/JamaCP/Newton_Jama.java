 /* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation                              
   */
/* Newton_Jama.java: Solve n nonlinear equations using Newton-Raphson and Jama 
Matrix library. Jama must be in same directory or CLASSPATH includes JAMA          */
 
import Jama.*;

public class Newton_Jama {

	public static void main(String[] argv)  {
    int n = 9, it, i;
		double deriv[][] = new double[n][n], dx[] = new double[n];
		double f[] = new double[n], errXi, errX, errF, eps = 1e-6;           // Precision
		double x[] = {0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1., 1., 1.};    // Initial Guess

		for ( it=1; it <= 100; it++ ) {            // Iteration loop, break when converge
      F(x,f);                               // Compute F vector = the RHS n equations
      dFi_dXj(x, deriv, n);                       // Compute derivative dFi/dXj array
 			Matrix A = new Matrix(deriv);                        // A = LHS matrix via Jama
			System.out.print("dFi_dXj for iteration " + it);                    
			A.print(7,4);                                       // Jama printout of dFi_dXj
      Matrix B = new Matrix(n,1);                                  // Update solution
			for ( i=0; i <= n-1; i++) B.set(i, 0, -f[i]);                  // RHS of system
			System.out.print("RHS vector [f(Xguess)]  for iteration " + it);   
			B.print(17,4); 
      Matrix sol = A.solve(B);                    // Solution vector = dx corrections
			System.out.print("dx corrections for iteration "  + it);
			sol.print(17,4);
			System.out.println (" x + dx for iteration " + it);
			for ( i=0; i <= n-1; i++) {
        dx[i] = sol.get(i,0);
				x[i]  = x[i] + dx[i];                                   // Increment solution
				System.out.println("x["+i+"] =  "+x[i]);
			}           
      System.out.println();                                // Printout final solution
      errX = errF = errXi = 0.0;     
      for ( i = 0; i <= n-1; i++) {                  //  Find max err, break if < eps
        if ( x[i] != 0.) errXi = Math.abs(dx[i]/x[i]);
          else errXi = Math.abs(dx[i]);
        if ( errXi > errX) errX = errXi;                            // Solution error
        if ( Math.abs(f[i]) > errF ) errF = Math.abs(f[i]);        // Functions error
	    }
     if ((errX <= eps) && (errF <= eps)) break;  // check for convergence
    }
  }
                                            // The eqtns;  enter your own system here
  public static void F(double x[], double f[])  { 
    f[0] = 3*x[3] + 4*x[4] + 4*x[5] - 8.0;
    f[1] = 3*x[0] + 4*x[1] - 4*x[2];
    f[2] = x[6]*x[0] - x[7]*x[1] - 10.0;
    f[3] = x[6]*x[3] - x[7]*x[4];
    f[4] = x[7]*x[1] + x[8]*x[2] - 20.0;
    f[5] = x[7]*x[4] - x[8]*x[5];
    f[6] = Math.pow(x[0],2) + Math.pow(x[3],2) - 1.0;
    f[7] = Math.pow(x[1],2) + Math.pow(x[4],2) - 1.0;
    f[8] = Math.pow(x[2],2) + Math.pow(x[5],2) - 1.0;
  }

  public static void dFi_dXj(double x[], double deriv[][], int n) {    // Derivatives
    double f[] = new double[n], temp, h = 1e-4;
		int i, j; 
		h  =   1e-4;                                              // Differentiation step
    for (j = 0; j <=  n-1; j++)  {                                // Loop for x + h/2
      temp = x[j];
			x[j] += h/2.;
			F(x, f);                                                 // Store F(x[j] + h/2)
      for (i = 0; i <=  n-1; i++)   deriv[i][j] = f[i]; 
      x[j] = temp;                                                      // Reset x[j]
    }
		for (j = 0; j <=  n-1; j++)  {                                // Loop for x - h/2
      temp = x[j];
			x[j] = x[j]-h/2.;
			F(x, f);                                                   // Store F(x[j]-h/2)
	    for (i = 0; i <=  n-1; i++) deriv[i][j] = (deriv[i][j] - f[i])/h;
      x[j] = temp;                                                      // Reset x[j]
    }
  }
}
