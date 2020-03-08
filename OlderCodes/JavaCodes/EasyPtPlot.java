/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// EasyPtPlot.java: Simple PtPlot application 
import ptolemy.plot.*; 

public class EasyPtPlot { 
  public static final double Xmin = -5., Xmax = 5.;                   // Graph domain
  public static final int Npoint = 500; 
  
  public static void main(String[] args) { 
    Plot plotObj = new Plot();                                  // Create Plot object
    plotObj.setTitle("f(x) vs x"); 
    plotObj.setXLabel("x"); 
    plotObj.setYLabel("f(x)"); 
 // plotObj.setSize(400, 300); 
 // plotObj.setXRange(Xmin, Xmax);  
 // plotObj.addPoint(int Set, double x, double y, boolean connect) 
    double xStep = (Xmax - Xmin) / Npoint; 
    for ( double x = Xmin;  x <= Xmax;  x += xStep) { 
      double y = Math.sin(x)*Math.sin(x); 
      plotObj.addPoint(0, x, y, true); 
    }
    PlotApplication app = new PlotApplication(plotObj);               // Display Plot
  } 
}
