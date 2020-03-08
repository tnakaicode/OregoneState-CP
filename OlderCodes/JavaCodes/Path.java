/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Path.java:        Parabolic Trajectory Class

public class Path {   
  public static final double g = 9.8;                       // Static, same all Paths
  public double v0x, v0y;                             // Non-static, unique each Path
    
  public Path(double v0, double theta)   {v0x = v0 * Math.cos(theta*Math.PI/180.); 
                                     v0y = v0 * Math.sin(theta * Math.PI / 180.);  }
      
  public double getX(double t) { return v0x * t; }
    
  public double getY(double t) { return v0y * t - 0.5*g*t*t; }
}
