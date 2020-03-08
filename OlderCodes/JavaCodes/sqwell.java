/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
* 
*  sqwell.java: Solves the time-dependent Schroedinger equation for a 
* Gaussian wavepacket in a infinite square well potential            
*  comment: Output data is saved in 3D grid format used by gnuplot. 
*/
import java.io.*;

public class sqwell  {

public static void main(String[] argv) throws IOException,
FileNotFoundException
{
  PrintWriter w = 
        new PrintWriter(new FileOutputStream("sqwell.dat"), true);
double psr[][]=new double[751][2];
double psi[][]=new double[751][2];
double p2[]=new double[751];
double dx,k0,dt,x,pi;
int i,n,max;
        max   = 750;
        pi   = 3.14159265358979323846;
        dx   = 0.02;
        k0   = 17.0*pi;
        dt   = dx*dx;   
//initial conditions
        x   = 0.0;
        for( i=0;i<max;i++){
/* 
 compute real part and imaginary part of the initial wave
 function using Euler's formula: 
 exp(i*k0*x)=cos(k0*x)+i*sin(k0*x)        
*/
//real part of the initial wave function at t=0         
psr[i][0] = Math.exp(-0.5*(2.0*Math.pow(x-5.0,2.0)))*Math.cos(k0*x);
//imaginary part of the initial wave function at t=0            
psi[i][0] = Math.exp(-0.5*(2.0*Math.pow(x-5.0,2.0)))*Math.sin(k0*x);
          x        = x + dx;
                              }

//now propagate solution through time

        

//the real part of the wave function and the probability        
          
for(n=0;n<6000;n++){
  for(i=1;i<max-1;i++){
psr[i][1] = psr[i][0] - dt*(psi[i+1][0] + psi[i-1][0]-2.0*psi[i][0])/(2.0*dx*dx);
p2[i]    = psr[i][0]*psr[i][1]+psi[i][0]*psi[i][0];             
                        }
//the imaginary part of the wave function    
         
        for(i=1;i<max-1;i++){
psi[i][1] = psi[i][0] + dt*(psr[i+1][1] + psr[i-1][1]-2.0*psr[i][1])/(2.0*dx*dx);  
                            } 
//only at certain time instants we want to know
//the probability
if(n%300==0) { 
            
  for(i=0;i<max;i=i+15){             
              
    w.println(""+p2[i]+"");
                       }
           w.println("");   
              }
//new iterations are now the old ones
          
 for(i=0;i<max;i++){           
            psi[i][0] = psi[i][1];
            psr[i][0] = psr[i][1];
                   }                   
}
 
      
        System.out.println("data saved in sqwell.dat");
        

}
} 