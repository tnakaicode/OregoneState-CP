/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/
/* ProjectileAir.java: Calculate projectile motion
Uses ptplot, by Edward A. Lee and Christopher Hylands */ 
 
import ptolemy.plot.*;
 
public class ProjectileAir
{
    // Static class variables, accessible to all methods (constants)
 static final double v0 = 22., angle = 34., g = 9.8, kf=0.8;
 static final int N = 25;

    public static void main(String[] args) 
    {
        double v0x,v0y, T, H, R;
        // Initial constants
        v0x = v0 * Math.cos(angle*Math.PI/180.);
        v0y = v0 * Math.sin(angle*Math.PI/180.);
        T = 2 * v0y / g;
        H = v0y * v0y / (2 * g);
        R = 2 * v0x * v0y / g;
        System.out.println("Frictionless T, H, R = " + T + ", " +H+ ", " +R);
        
        // Create a Plot object (from the ptplot package).
        Plot myPlot = new Plot();
        myPlot.setTitle("Projectile without/with Air Resistance");
        myPlot.setXLabel("x");
        myPlot.setYLabel("y)");
        myPlot.setXRange(-R/20, R);
        
  plotAnalytic (myPlot, 0);      // Plot, no friction, analytic 
  plotNumeric  (myPlot, 2, kf);  // Plot, friction, numeric
        
        //PlotApplication display Plot object.
        PlotApplication app = new PlotApplication(myPlot);

    } //end main

  public static void plotNumeric (Plot myPlot, int set, double k)
    {  
  //Plot, friction, numeric
  double x, y, vx, vy, dt;
        vx = v0 * Math.cos(angle*Math.PI/180.);
        vy = v0 * Math.sin(angle*Math.PI/180.);
        x = 0;
        y = 0;
  dt = 2 * vy / g /N;
        for (int i = 0; i<N; i=i+1)
        {
            vx = vx -k * vx * dt;
            vy = vy - g * dt - k * vy * dt;
            x = x + vx * dt;
            y = y + vy * dt;
            myPlot.addPoint(set, x, y, true);
            System.out.println("x = " + x   + " y = " + y); 
        }
  myPlot.addPoint(set, x, y, true);
    } //end plotNumeric

public static void plotAnalytic (Plot myPlot, int set)
    {  
  //Plot, frictionless, analytic
  double x, y, v0x, v0y, dt, t;
        v0x = v0 * Math.cos(angle*Math.PI/180.);
        v0y = v0 * Math.sin(angle*Math.PI/180.);
  dt = 2 * v0y / g /N;
        for (int i = 0; i<N; i=i+1)
        {
            t = i * dt;
            x = v0x * t;
            y = v0y * t - g * t * t / 2.;
            myPlot.addPoint(set, x, y, true);
            System.out.println("Frictionless x = " + x   + " y = " + y); 
        }
    } // end plotAnalytic

}  //end class
