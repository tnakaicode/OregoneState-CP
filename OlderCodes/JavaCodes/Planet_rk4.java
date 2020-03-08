/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/
import java.awt.*;
import java.applet.Applet;

public class Planet_rk4 extends Applet
{
// FUNCTION of your choice below

public static void f(double t, double y[], double F[])
//definition of equation
{   double r, denomTimesr;
    r=Math.sqrt(y[0]*y[0]+y[2]*y[2]);
    denomTimesr = Math.pow(r,3);
    F[0] = y[1];  // RHS of first equation
    F[1] =  -y[0]/denomTimesr;// RHS of second equation
    F[2] = y[3];  // RHS of third equation
    F[3] =  -y[2]/denomTimesr;// RHS of 4th equation
}
// ====== rk4 method below is *NOT TO BE MODIFIED. Instead, modify f method
// this method advances the N-vector solution ahead by one step.
public static  void rk4(double t, double y[], double h, int Neqs)
    {
    int i;
    double F[] = new double[Neqs]; 
    double ydumb[] = new double[Neqs];
    double k1[] = new double[Neqs]; double k2[] = new double[Neqs]; 
    double k3[] = new double[Neqs]; double k4[] = new double[Neqs];  
    
    f(t, y, F);
    for (i=0; i<Neqs; i++)
        { k1[i] = h*F[i]; 
         ydumb[i] = y[i] + k1[i]/2;}
     
    f(t + h/2, ydumb, F);
    for (i=0; i<Neqs; i++) 
        { k2[i] = h*F[i];
         ydumb[i] = y[i] + k2[i]/2;}
        
    f(t + h/2, ydumb, F);
        for (i=0; i<Neqs; i++) 
        { k3[i]=  h*F[i];
         ydumb[i] = y[i] + k3[i];}
        
    f(t + h, ydumb, F);
    for (i=0; i<Neqs; i++) 
    { k4[i] = h*F[i];
     y[i] = y[i] + (k1[i] + 2*(k2[i]+k3[i]) + k4[i])/6;}
}


public void paint(Graphics g)
{
double h=0.1, t=0.;
double y[] = new double[4]; 
int n;

g.setColor(Color.blue);
g.drawString("PLANET MOTION",150,50);
g.setColor(Color.yellow);
g.fillOval(280,130,10,10);
g.setColor(Color.red);
//y0=x.y1=x',y2=y,y3=y'
y[0]=0.5; y[2]=0.0; y[1]=0.0; y[3]=1.63 ;
for (n=1; n<=1;n++)
{
rk4(t, y, h, 4);
System.out.println ("output values: vx, vy =  "+y[1] +", "+y[3]);
System.out.println ("output values: x, y =  "+y[0] +", "+y[3]);
g.fillOval((int)((619*y[0]/3)+270),(int)(100-99*(y[2]-0.5)/1.5),5,5);

try{
    Thread.sleep(100);
}catch (InterruptedException e){};


}
for (n=2; n<=410;n++)
{
 rk4(t,  y,  h, 4);
//System.out.println ("output values: vx, vy =  "  + y[1] +", " + y[3]);
//System.out.println ("output values: x, y =  "  + y[0] +", " + y[3]);
g.fillOval((int)((619*y[0]/3)+270),(int)(100-99*(y[2]-0.5)/1.5),5,5);

try{
    Thread.sleep(100);
}catch (InterruptedException e){};


}
}
}
