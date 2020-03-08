/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/
/*       * MD2D.java applet
*   Molecular dynamics for n atoms in 2D with  Lennard-Jones potential,
*   velocity Verlet algorithm 
*   Output average pressure, average temperature , average kinetic energy and average 
*   potential energy.
*   Paint and animate atoms in canvas. Use buffering in order to avoid flicker.
*   
***********************************************************************

* Natom - number of atoms 
* Nmax - maximum number of atoms  
* Nstep - number of time steps 
* Nprint - number of time steps between printing
* L   = box size
* h - time step 
* hover2 = h/2
* PE - potential energy  
* KE - kinetic energy    
* T - temperature
* Press - pressure
* fx[],fy[],fz[] - forces 
* x[],y[],z[] - positions 
* vx[],vy[],vz[] - velocitys
* w - virial
*******************************************************************/
import java.io.*;
import java.util.*;
import java.awt.*;
import java.applet.Applet; 
import java.awt.event.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent; 

 
 public class MD2Df extends Applet

{

Image imageBuff = null;
Graphics graphics;
//background color
Color bgColor = Color.white;
//atoms color
Color atomColor = Color.red;
// image buffer dimensions
static int width = 400;
static int height = 300;
//text field to output pressure
TextField tx1=new TextField(30); 
//text field to output kinetic energy
TextField tx2=new TextField(30);
//text field to output potential energy
TextField tx3=new TextField(30);
//text field to output temperature
TextField tx4=new TextField(30);     

  
  //Class variables used by Forces Method
static  int L=6, Natom=30, Nmax=513;      
static double x[]=new double[Nmax], y[]=new double[Nmax];  
static double fx[][]=new double[Nmax][2], fy[][]=new double[Nmax][2];



public void init(){
// add text fields
add(tx1);   
add(tx2);
add(tx3);
add(tx4);
 

setBackground(Color.lightGray);



}

public void paint(Graphics g) {
     update(g);
}


public void update(Graphics g)

{


int  t1, t2, i, Itemp, t,  Nstep=5000;
 int  Nprint=100;
 double h=0.004, hover2,  PE, KE, P, T, wij, eKavg, ePavg; 
 
 double Tinit=2.0; 
 
 double avT=0.0,Tavg=0.0, avP=0.0;
 double Pavg=0.0, avKE=0.0, avPE=0.0;
 double dens=1.00; //density (1.20 for fcc)
 

 double vx[]=new double[Nmax];
 double vy[]=new double[Nmax];

 //double wij[]=new double[Nmax];  

 //L = (int)Math.pow(1.*Natom, 1/3.); 
// Natom =  (int)Math.pow(L, 3);
 
 System.out.println("Natom = "+Natom+" L= "+L+"");
       int ix,iy;
       i = -1;
//Set up the initial lattice configuration      
       for(ix = 0; ix<=L-1; ix++){
       for(iy = 0; iy<=L-1; iy++){
       
       i = i+1;
       x[i] = ix;
       y[i] = iy;            
       
       
 
 //Initial velocities according to Maxwell-Gaussian Distribution

       vx[i] =(Math.random()+Math.random()+Math.random()+
                    Math.random()+Math.random()+Math.random()+
                    Math.random()+Math.random()+Math.random()+
                    Math.random()+Math.random()+Math.random())/12.-0.5;
        vy[i] =(Math.random()+Math.random()+Math.random()+
                    Math.random()+Math.random()+Math.random()+
                    Math.random()+Math.random()+Math.random()+
                    Math.random()+Math.random()+Math.random())/12.-0.5;
  
 
  vx[i] = vx[i]*Math.sqrt(Tinit); //scale velocity with temperature
  vy[i] = vy[i]*Math.sqrt(Tinit);
  

         } }

 //t, t+h indices
 t1 = 0;
 t2 = 1;
 hover2 = h/2.0;
 //w = 0.0;//initial virial
 PE = 0.0; double w = 0.0;
 // initial KE & PE via Forces
 t = 0;
 KE = 0.0;
 for(i = 0; i <= Natom-1; i++) KE=KE+(vx[i]*vx[i]+vy[i]*vy[i])/2;

//System.out.println(""+t+" PE= "+PE+" KE = "+KE+" PE+KE = "+(PE+KE));

PE = Forces(t1,w,PE,1);

//System.out.println(""+t+" PE= "+PE+" KE = "+KE+" PE+KE = "+(PE+KE));
//Main loop
     for( t = 0; t < Nstep; t++ ){ 
     
     for( i = 0; i <= Natom-1; i++ ){ 
     
     
     
     PE = Forces(t1,w,PE,1);
     // velocity Verlet algorithm
         x[i] = x[i] + h*(vx[i] + hover2*fx[i][t1]);
         y[i] = y[i] + h*(vy[i] + hover2*fy[i][t1]);
         
      //periodic boundary conditions
      if (x[i] <= 0.) x[i] = x[i] + L;
      if (x[i] >= L)  x[i] = x[i] - L;
      if (y[i] <= 0.) y[i] = y[i] + L;
      if (y[i] >= L)  y[i] = y[i] - L;
     
    }
    
    PE = 0.;
     PE = Forces(t2, w, PE, 1);
     KE = 0.;  w = 0.;
     for( i = 0; i <= Natom-1; i++){ 
            vx[i] = vx[i] + hover2*(fx[i][t1] + fx[i][t2]);
            vy[i] = vy[i] + hover2*(fy[i][t1] + fy[i][t2]);
            
       

       KE = KE + (vx[i]*vx[i] + vy[i]*vy[i])/2;
      }
     
     w = Forces(t2, w, PE, 2);
     //P = dens*(2.*KE+w)/3.;
     P=dens*(KE+w);
     //T = 2.*KE / (3.*Natom);
 T = KE / (Natom);
   // g.setColor(Color.blue); 
//tx1.setBounds(425, 60, 129, 20);
//tx2.setBounds(425, 120, 129, 20);
//tx3.setBounds(425, 180, 129, 20);
//tx4.setBounds(425, 240, 129, 20);
//g.drawString("Pressure",425,40);

//g.drawString("Kinetic Energy",425,100);

//g.drawString("Potential Energy",425,160);

//g.drawString("Temperature",425,230);

//g.setColor(Color.black); 
//tx1.setText("P= "+(float)P);
//tx2.setText("KE= "+(float)KE);
//tx3.setText("PE= "+(float)PE);
//tx4.setText("T= "+(float)(T);

     //T=KE/Natom; 
//increment averages
        avT = avT + T ;  //Temperature
        avP = avP + P; //Pure 
        avKE = avKE + KE;  //Kinetic energy 
        avPE = avPE + PE;   //Potential energy
        
        if (t%Nprint == 0) 
        {if (t==0) t=1;
 
    Pavg = avP /t; 
    eKavg = avKE /t ;
    ePavg = avPE /t;
    Tavg = avT /t;

g.setColor(Color.blue); 
tx1.setBounds(425, 60, 149, 20);
tx2.setBounds(425, 120, 149, 20);
tx3.setBounds(425, 180, 149, 20);
tx4.setBounds(425, 240, 149, 20);   
   
g.drawString("Average Pressure",425,40);

g.drawString("Average Kinetic Energy",425,100);

g.drawString("Average Potential Energy",425,160);

g.drawString("Average Temperature",425,230);

g.setColor(Color.black); 
tx1.setText("P= "+(float)Pavg);
tx2.setText("KE= "+(float)eKavg);
tx3.setText("PE= "+(float)ePavg);
tx4.setText("T= "+(float)Tavg);




  }      
//create buffer
if (imageBuff == null) {
       imageBuff = createImage(width,height);
       if(imageBuff == null) {
      System.out.println("image = null!!");
      System.exit(0);
       }
       graphics = imageBuff.getGraphics();
   }


// draw into buffer and then output to the canvas.
// draw background.
graphics.setColor(bgColor);
graphics.fillRect(0,0,width,height);

// draw atoms
graphics.setColor(atomColor);
for(i=0;i<=Natom-1;i++){



graphics.fillOval((int)(719*x[i]/15),(int)(100-99*(y[i]-5)/3.5),10,10);




    
      }
       
       
       Itemp = t1; //time t and t+h
       t1 = t2;
       t2 = Itemp;
       // output the buffer to canvas
       g.drawImage(imageBuff,0,0,this);
    
    }

}

/*************************************************************
*   Forces function
*    Compute forces, PE, and Virial; 
*    return PE or w as function value, Forces via argument
*        V(r) = 4*(1/r**12 - 1/r**6)
**************************************************************/
public static double Forces(int t, double w, double PE, int PEorW)
{   
int i, j;
double fijx,fijy,wij;
double r2, invr2=0.0, dx, dy,r2cut;
r2cut = 9.;// Cut-off radius

// Initialize forces.
 PE = 0.;
 for (i=0; i<= Natom-1; i++) {fx[i][t] = fy[i][t]=0.0 ;}
 wij = 0.;
//Compute forces.
      for(i = 0; i<= Natom-2; i++){
          for(j = i+1; j<=Natom-1; j++){
  dx = x[i]-x[j];
  dy = y[i]-y[j];
 
//minimum image criterium
   if(Math.abs(dx) > 0.50*L) {dx = dx - sign(L,dx);}
   if(Math.abs(dy) > 0.50*L) {dy = dy - sign(L,dy);}
   //System.out.println("Natom = "+Natom+" L= "+L+"");
   r2 = Math.pow(dx,2)+Math.pow(dy,2);
// Cut off
   if(r2 < r2cut){
       if(r2==0.) r2=0.0001;
       invr2 = 1./r2;   
       wij =  48.*(Math.pow(invr2,3)-0.5)*Math.pow(invr2,3);    
       fijx = wij*invr2*dx;
       fijy = wij*invr2*dy;   
     
       fx[i][t] = fx[i][t] + fijx;
       fy[i][t] = fy[i][t] + fijy;
       
       fx[j][t] = fx[j][t] - fijx;
       fy[j][t] = fy[j][t] - fijy;
        
       PE = PE + 4*Math.pow(invr2,3)*( Math.pow(invr2,3) - 1.);
       w = w + wij;     
      }
  }
    }
      if (PEorW == 1) return PE;     
      else return w;     
}
public static double  sign(double a,double b){
    if (b >= 0.) return Math.abs(a);
    else  return -Math.abs(a); }      


public void actionPerformed(ActionEvent e) { 
   

                                 
      
     repaint();
   

 }

}
