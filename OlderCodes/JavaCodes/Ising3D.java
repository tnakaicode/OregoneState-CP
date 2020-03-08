/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*  
*   Ising3D.java:Ising 3D model of magnetic dipol string 
*/
import java.io.*;
import java.util.*;
public class Ising3D
  {
public static void main(String[] argv)throws IOException,
FileNotFoundException
{
  PrintWriter q = 
        new PrintWriter(new FileOutputStream("specific_heat.dat"), true);
    PrintWriter p = 
        new PrintWriter(new FileOutputStream("susceptibility.dat"), true);
  PrintWriter s = 
        new PrintWriter(new FileOutputStream("energy.dat"), true);
    PrintWriter t = 
        new PrintWriter(new FileOutputStream("magnetization.dat"), true);
   int m=100,i,j,k,l,N,n;
   double sum,E, M,M2,U,U2,C,Chi;
     double Energy,Magnetization,kT;
//Number of try
  N=2001;
//Lattice size
 int Ni=20, Nj=20, Nk=20;
//Spin configuration
double a[][][]=new double[Ni+1][Nj+1][Nk+1];
// Random number generator's seed
  int seed=734568980;
  Random r=new Random(seed);     
// Temperature
 for(kT=0.5;kT<8.0;kT+=0.10){ 
//Running Averages    
// Energy
      U  = 0.0;
      U2 = 0.0;
// Magnetization 
       M  = 0.0;
       M2 = 0.0;
// Set up the initial lattice configuration, 
      for( i = 0;i<Ni-1;i++){
       for( j = 0;j<Nj-1;j++){
        for( k = 0;k<Nk-1;k++){
        a[i][j][k] = 1.0;
}  }  }
// Initial Energy and magnetization

       Energy = 0.0;
       Magnetization = 0.0;
       for( i = 0;i<Ni-1;i++){
       for( j = 0;j<Nj-1;j++){
       for( k = 0;k<Nk-1;k++){
        sum=0.0;//sum of nearest neighbors
// Periodic Boundary Conditions
        if(i>1 ) sum+=a[i-1][j][k];
        if(i<Ni) sum+=a[i+1][j][k];
        if(j>1 ) sum+=a[i][j-1][k];
        if(j<Nj) sum+=a[i][j+1][k];
        if(k>1 ) sum+=a[i][j][k-1];
        if(k<Nk) sum+=a[i][j][k+1];
        Energy=Energy - sum * a[i][j][k]; 
        Magnetization = Magnetization - a[i][j][k];
      }
    }
}
//  Correct the energy
      Energy=Energy/2.0;
     for( l = 1;l< N;l++){
      for( n = 1;n<Ni*Nj*Nk;n++){
//Pick the node to flip at random
         i = (int)((r.nextDouble()*Ni))+1; 
         j = (int)((r.nextDouble()*Nj))+1;
         k = (int)((r.nextDouble()*Nk))+1;

// Calculate  change in energy  when flipped
         sum=0.0;
        if(i>1 ) sum+=a[i-1][j][k];
        if(i<Ni) sum+=a[i+1][j][k];
        if(j>1 ) sum+=a[i][j-1][k];
        if(j<Nj) sum+=a[i][j+1][k];
        if(k>1 ) sum+=a[i][j][k-1];
        if(k<Nk) sum+=a[i][j][k+1];
      
        E =  2.0 * sum * a[i][j][k]; //energy change

// Metroplis criterium
     
     if(( E<0.0 )||( Math.exp(-E/kT)>r.nextDouble() )) 

//If accept the move upgrade spin, total magnetization, and energy
             { 
             
             a[i][j][k]=-a[i][j][k]; //flip the spin
             Energy = Energy + E;
             Magnetization = Magnetization - 2.0 * a[i][j][k];}
      }
// Increment averages
         M = M + Magnetization;
         M2= M2+ Magnetization * Magnetization;
         U = U + Energy ;
         U2= U2+ Energy * Energy;
// Print out to files
     if((l%(N-1)) == 0)
     {
             U= U/ l/Ni/Nj/Nk;//average energy
             U2 = U2 / l/Ni/Nj/Nk/Ni/Nj/Nk;
             C = ((U2-U*U))/kT/kT;//specific heat
             M = M / l /Ni/Nj/Nk;//average magnetization
             M2 = M2 / l /Ni/Nj/Nk/Ni/Nj/Nk;
             Chi = ((M2-M*M))/kT;//susceptibility   
  System.out.println(" "+kT+" "+C+" ");  
   p.println(" "+kT+" "+C+" "); // print to file the specific heat
   q.println(" "+kT+" "+Chi+" "); // print to file the susceptibility
   s.println(" "+kT+" "+U+" "); // prin to file the average energy
   t.println(" "+kT+" "+Math.abs(M)+" "); // print to file the average magnetization
} }}}} 