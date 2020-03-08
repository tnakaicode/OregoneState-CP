/*
************************************************************************
*  Slit.java: Solves the time-dependent Schroedinger equation for a    *
*   two-dimensional Gaussian wavepacket entering a slit                *
*                                                                      *
*  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*                                                                      *
*  comment: Output data is saved in 3D grid format used by gnuplot.    *
*                                                                      *
************************************************************************
 */
import java.io.*;
public class Slit{


public static void main(String[] argv) throws IOException,
FileNotFoundException
{
    PrintWriter w = 
        new PrintWriter(new FileOutputStream("slit.dat"), true);
double psr[][][]=new double[91][91][2];
double psi[][][]=new double[91][91][2];         
double v[][]=new double[91][91];        
double p2[][]=new double[91][91];       
double a1,a2,dt,dx,k0x,k0y,x0,y0,x,y;
int i,j,max,n,time; 
           
// Enter a positive integer from 1(initial time)
//to 800 to get wave packet position at that time
time=800;     
//initializes the constant values and the wave packet      
        
dx   = 0.2;
dt  = 0.0025/(dx*dx);
//initial momentum, position        
        k0x  = 0.0;
        k0y  = 2.5;     
        x0   = 0.0;
        y0   = -7.0;
        max  = 90;
//initial wave function
        y   = -9.0;
    for( j=0;j<max;j++){
    
          x=-9.0;
          for( i=0;i<max;i++){
 /*         
 compute real part and imaginary part of the initial wave
 function using Euler's formula: 
 exp(i*(k0x*x+k0y*y))=cos(k0x*x+k0y*y)+i*sin((k0x*x+k0y*y)             
*/
a1 = Math.exp(-0.5*(Math.pow(x-x0,2.0)+Math.pow(y-y0,2.0)));
//real part of the initial wave function
            psr[i][j][0] = a1*Math.cos(k0x*x+k0y*y);
//imaginay part of the initial wave function
            psi[i][j][0] = a1*Math.sin(k0x*x+k0y*y);
            x          = x + dx;
                              }
          y = y + dx;
                         }   
//set up the potential slit width: 50-40=10 units      

              
          
 for( j=0;j<max;j++){           
           for( i=0;i<max;i++){ 
  if((j==33)&&((i<=39)||(i>=50)))
        v[i][j] = 0.5;       
            else  
              v[i][j] = 0.0;
                                         
                               } 
                     }

//propagate solution through time     
for(n=0;n<time;n++){
      
//compute real part of wave packet and probability              
          
for(j=1;j<max-1;j++){ 
            
    for(i=1;i<max-1;i++){     
     a2 = v[i][j]*psi[i][j][0]+2.0*dt*psi[i][j][0];
     a1 = psi[i+1][j][0]+psi[i-1][j][0]+psi[i][j+1][0]+psi[i][j-1][0];
     psr[i][j][1] = psr[i][j][0]-dt*a1+2.0*a2;
     p2[i][j] = psr[i][j][0]*psr[i][j][0]+psi[i][j][0]*psi[i][j][1];  
                          
                         }  
//at x edges derivative is zero  
       psr[0][j][1] = psr[1][j][1]; 
       psr[max][j][1] = psr[max-1][j][1];                  
                       }   
//imaginary part of wave packet is next                  
          
 for(j=1;j<max-1;j++){ 
    for(i=1;i<max-1;i++){            
            
     a2 = v[i][j]*psr[i][j][1]+2.0*dt*psr[i][j][1];
     a1 = psr[i+1][j][1]+psr[i-1][j][1]+psr[i][j-1][1]+psr[i][j+1][1];
     psi[i][j][1] = psi[i][j][0]+dt*a1-2.0*a2;   
                         }  
//at x edges derivative is zero  
     psi[0][j][1]    = psi[1][j][1]; 
     psi[max][j][1] = psi[max-1][j][1];                  
                      }         
//new iterations are now the old ones, recycle
          
for(j=1;j<max-1;j++){ 
    for(i=1;i<max-1;i++){                
     psi[i][j][0] = psi[i][j][1];
     psr[i][j][0] = psr[i][j][1];
                        }   
                     }                  
              }   

//write the probabilities plus potential
//in disk file, to give a volume sensation of the potential 

 for(j=0;j<max;j=j+3){ 
      for(i=0;i<max;i=i+2){            
            w.println(""+(p2[i][j]+v[i][j])+""); 
                          }
   
            w.println("");        
                        }
        
        
        System.out.println("data saved in slit.dat");   
         
}
}
