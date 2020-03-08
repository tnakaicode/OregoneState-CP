/* 
************************************************************************
*   HeatCNTridiag.c Solution of heat equation :
  du(x,t)/dt = ct*ct *d2u(x,t)/dx2 where ct*ct=K/C*ro                
  using  Crank-Nicholson method                                        *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; & CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
*                                                                      *
************************************************************************
*/

#include <stdio.h>
#include <math.h>
#define Max 51
 main()
{
  
  FILE *output;
  output=fopen("HeatCNTridiag.dat","w");
  
  
    	/* save data in HeatCN2.dat */ 
    
    double Ta[Max],Tb[Max],Tc[Max],Td[Max],a[Max],b[Max],c[Max],d[Max],x[Max],t[Max][Max];


     /* grid-amplitudes*/
    int i, j;                /* loop counters */
    double width=1.0, height=0.1;  /*  width and height of rectangle */
    int n=50, m=50;                /* number of grid points */ 
    void Tridiag(double a[],double d[],double c[],double b[],double Ta[],double Td[],double Tc[],double Tb[],double x[],int n);
double ct=1.0;        /* constants */
    double k, r;     /* constants*/ 
    
    double h;                /* step size  */
    double Pi = 3.1415926535;
    /*initialize*/
    for ( i = 0; i <= n; i++ ) t[i][0] = 0.0;
    for ( i = 0; i <= m; i++ ) t[0][i] = 0.0;
    /* compute step sizes */
     h  = width / ( n - 1 );
    /* compute r and constants */

    k  = height / ( m - 1 );
    r  = ct * ct * k / ( h * h );
     /* boundary conditions */

    for ( j = 0; j <= m-1; j++ ) 
    {
           t[0][j] = 0.0;
           t[n-1][j] = 0.0;
    }

    
    /*initial conditions*/
    for ( i = 0; i <= n-1  ; i++ ) 
    
    t[i][0] = sin( Pi * h *i);
      
    /* diagonal elements of the matrix A  */

    for ( i = 0; i <= n-1 ; i++ ) 
    Td[i] = 2.0 + 2.0 / r;
    Td[0] = 1.0;
    Td[n-1] = 1.0;

    /* off diagonal elements of the matrix A */

    for ( i = 0; i <= n - 1 ; i++ ) 
    {
        Ta[i] = -1.0;
        Tc[i] = -1.0;
    }
       
    Ta[n-1] = 0.0;
    Tc[0]   = 0.0;

    Tb[0] = 0.0;
    Tb[n-1] = 0.0;

    for ( j = 1; j <= m-1; j++ )
    {
        for ( i = 1; i <= n - 2; i++ ) 
        {
         Tb[i] = t[i-1][j-1] + t[i+1][j-1] + (2.0 / r  - 2.0) * t[i][j-1];
        }
        /* solve the dridiagonal system. */
        Tridiag(a,d,c,b,Ta,Td,Tc,Tb,x,n); 
         
         for ( i = 0; i <= n-1; i++ ) t[i][j] = x[i];

        }
   /* write data gnuplot 3D format */
     
       for (j=0;j<=m-1;j++)
       { for ( i = 0; i <= n-1; i++ )
          fprintf(output,"%f \n",t[i][j]);   
          fprintf(output,"\n");  /* empty line for gnuplot */
       }
      fclose(output);
  
} /* solve the tridiagonal system. */

void Tridiag(double a[],double d[],double c[],double b[],double Ta[],double Td[],double Tc[],double Tb[],double x[],int n)
{ 
  int i;
//int Max=51;
  double h[Max], p[Max];
  for ( i = 0; i <= n-1; i++ ) {
  a[i] = Ta[i];    /* vector replacement first */
  b[i] = Tb[i];
  c[i] = Tc[i];
  d[i] = Td[i];
  }
  h[0]=c[0]/d[0];p[0]=b[0]/d[0];
  for ( i = 1; i <= n-1; i++ ) 
        {
        	
        h[i]=c[i]/(d[i]-a[i]*h[i-1]);
        p[i]=(b[i]-a[i]*p[i-1])/(d[i]-a[i]*h[i-1]);
        
        	
        
        }

  x[n-1] = p[n-1];
  for ( i = n - 2; i >= 0; i-- ) 
  x[i] = p[i] - h[i]*x[i+1];
  }

