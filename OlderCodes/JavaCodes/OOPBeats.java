/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//   OOPBeats.java:  OOP Superposition 2 Sine waves
import ptolemy.plot.*; 

public class OOPBeats {   
  public double A, k1, k2; 
                                                                 // Class Constructor
  public OOPBeats(double Ampl, double freq1, double freq2)
		{ A = Ampl;  k1 = freq1;  k2 = freq2; }
   
  public void sumwaves()  {                                           // Sums 2 waves
    int i; 
    double y1, y2, y3, x = 0; 
    Plot myPlot = new Plot(); 
    myPlot.setTitle("Superposition of two Sines"); 
    myPlot.setXLabel(" x"); 
    for ( i = 1; i < 501; i++ ) {
      y1 = A*Math.sin(k1*x);                                              // 1st Sine
      y2 = A*Math.sin(k2*x);                                              // 2nd Sine
      y3 = y1 + y2;                                                   // Superpositon
      myPlot.addPoint(0, x, y3, true); 
      x = x + 0.01;                                                    // Increment x
    }
    PlotApplication app = new  PlotApplication(myPlot); 
  }
  
  public static void main(String[] argv)  {                         // Class instance
    OOPBeats sumsines = new OOPBeats(1., 30., 33.);                       // Instance
    sumsines.sumwaves();                                      // Call sumsins' method
  }
}
