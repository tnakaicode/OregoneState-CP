/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC BORDEIANU, Univ Bucharest, 2007
   Support by National Science Foundation      
*/
//  Lyapunov exponents for Lorenz's system of equations based on:   
//  A. Wolf, J. B. Swift, H. L. Swinney, and J. A. Vastano,                  
//  "Determining Lyapunov Exponents from a Time Series,"Physica D,            
//  Vol.16,pp.285-317,1985.                                                  

//n=number of nonlinear odes
//nn=n*(n+1)=total number of odes
//nn1=nn+1 
//(n=3, nn=12, nn1=13)

 
import java.io.*;
import java.util.*;

public class Lyap_Lorenz{
	public static int n=3; public static int nn=12; public static int nn1=13;
	public static double t; 
    public static double y[]= new double[nn1+1];
    public static double v[]= new double[nn1+1];
    
public static void main(String[] argv) throws IOException,
FileNotFoundException
{
    double lyap[]=new double[n+1];
    double cum[]= new double[n+1];// running sums
    double znorm[]= new double[n+1];// vector's norm ( magnitude)
    double gsc[]= new double[n+1];// GSR coefficients (the inner products)
    t = 0.0;
    int i,j,k,l,m;   
// output    
  PrintWriter w =new PrintWriter(new FileOutputStream("lyap1.dat"), true);
  PrintWriter q =new PrintWriter(new FileOutputStream("lyap2.dat"), true);
  PrintWriter r =new PrintWriter(new FileOutputStream("lyap3.dat"), true);
    
//      initial conditions for nonlinear ODEs
//      (** Choose within the system's basin of attraction **)
        v[1]=1.0;
        v[2]=1.0;
        v[3]=1.0;
       double Tmin=0.001;// initial time
       double Tmax=100.0; // final time
       double h=0.001;// time step
//      initial conditions for linearized ODEs
//      (** Leave these #s alone! They are problem independent! **)
//
        for(i=n;i<=nn;i++){
           v[i]=0.0;
        }
      
        for(i=1;i<=n;i++){
           v[(n+1)*i]=1.0;
           cum[i]=0.0;
        }
        
       
for(t = Tmin; t <= Tmax; t += h){

		rk4(t, v, h, nn);
          
//      Construct a new orthonormal basis by Gram-Schmidt method
//      Normalize first vector

        znorm[1]=0.0;
        for(j=1;j<=n;j++){
           znorm[1]+=Math.pow(v[n*j+1],2);
        }
        znorm[1]=Math.sqrt(znorm[1]);
        for(j=1;j<=n;j++){
           v[n*j+1]=v[n*j+1]/znorm[1];
        }

//      Generate the new orthonormal set of vectors

//      Generate  j-1 GSR coefficients (the inner products)

        for(j=2;j<=n;j++){

        for(k=1;k<=j-1;k++){
           gsc[k]=0.0;
           for(l=1;l<=n;l++){
              gsc[k]+=v[n*l+j]*v[n*l+k];
           }
        }
//       Construct new vectors

        for(k=1;k<=n;k++){
           for(l=1;l<=j-1;l++){
              v[n*k+j]=v[n*k+j]-gsc[l]*v[n*k+l];
           }
        }

//       Calculate the vector's norm

        znorm[j]=0.0;
        for(k=1;k<=n;k++){
           znorm[j]=znorm[j]+Math.pow(v[n*k+j],2);
        }
        znorm[j]=Math.sqrt(znorm[j]);
        
//       Normalize the new vectors

        for(k=1;k<=n;k++){
           v[n*k+j]=v[n*k+j]/znorm[j];
        }
   }
//      Update running vector magnitudes

        for(k=1;k<=n;k++){
           cum[k]=cum[k]+Math.log(znorm[k]);
           lyap[k]=cum[k]/t;
        }
   
//      Normalize exponent and print
         w.println(" "+t+" "+cum[1]/t+" ");
         q.println(" "+t+" "+cum[2]/t+" ");
         r.println(" "+t+" "+cum[3]/t+" ");
   }
System.out.println("data saved in lyap1.dat,lyap2.dat and lyap3.dat");
}

//***************************************************************
 public static double f(double t,double y[],int s){

 //parameters for Lorenz ODEs
       int i;
       double b=8.0/3.0;
       double sg =10.0;
       double r=28.0;
        //double b=4.0;
        //double sg =16.0;
        //double r=45.92;

 //      Nonlinear Lorenz equations

     if(s==1)   return sg*(y[2]-y[1]);
     if(s==2)   return -y[1]*y[3]+r*y[1]-y[2];
     if(s==3)   return y[1]*y[2]-b*y[3];
        
//       Linearized Lorenz equations
     if(s==4)   return sg*(y[7]-y[4]);
     if(s==5)   return sg*(y[8]-y[5]);
     if(s==6)   return sg*(y[9]-y[6]);
	 if(s==7)   return (r-y[3])*y[4]-y[7]-y[1]*y[10];
	 if(s==8)   return (r-y[3])*y[5]-y[8]-y[1]*y[11];
	 if(s==9)   return (r-y[3])*y[6]-y[9]-y[1]*y[12];
	 if(s==10)  return y[2]*y[4]+y[1]*y[7]-b*y[10];
	 if(s==11)  return y[2]*y[5]+y[1]*y[8]-b*y[11];
	 else       return y[2]*y[6]+y[1]*y[9]-b*y[12];
	
}
//***************************************************************
public static void rk4(double t, double y[], double h,int N)
	{
	
	double t1[] = new double[N+1];  //temporary storage
	double t2[] = new double[N+1];
	double t3[] = new double[N+1];
	double k1[] = new double[N+1];  //for Runge-Kutta
	double k2[] = new double[N+1];
	double k3[] = new double[N+1];
	double k4[] = new double[N+1];	
	int i=0;
	for(i = 1;i<=N;i++) {
		k1[i] = h*f(t,y,i);
		t1[i] = y[i]+0.5*k1[i];
		}
	
	for(i = 1;i<=N;i++){
		k2[i] = h*f(t+h/2.,t1,i);
		t2[i] = y[i]+0.5*k2[i];
		}
	
	for(i = 1;i<=N;i++){
		k3[i] = h*f(t+h/2.,t2,i);
		t3[i] = y[i]+k3[i];
		}

	for(i = 1; i<=N; i++)
		k4[i] = h*f(t+h,t3,i);	
	
	for(i = 1; i<=N; i++)
		y[i]+= (k1[i]+2.0*k2[i]+2.0*k3[i]+k4[i])/6.0;
	}

}