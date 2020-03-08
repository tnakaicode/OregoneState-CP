/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//   Moon.java:  moon orbiting a planet
import ptolemy.plot.*; 

public class Moon {  
  
  public static void main(String[] argv) {  
    double Radius, wplanet, radius, wmoon, time, x, y;                                          
    Radius = 4. ;                                                           // Planet
    wplanet = 2. ;                                                 // Omega of planet
    radius = 1. ;                                                   // Moon's orbit r
    wmoon = 14. ;                                            // Oemga moon wrt planet
    Plot myPlot = new Plot(); 
    myPlot.setTitle("Motion of a moon around a planet "); 
    myPlot.setXLabel(" x"); 
    myPlot.setYLabel(" y"); 
    for ( time = 0. ; time < 3.2; time = time + 0.02)  {   
      x = Radius *Math.cos(wplanet*time) + radius*Math.cos(wmoon*time); 
      y = Radius *Math.sin(wplanet*time) + radius*Math.sin(wmoon*time); 
      myPlot.addPoint(0, x, y, true);  
    }
    PlotApplication app = new  PlotApplication(myPlot); 
  }
}
