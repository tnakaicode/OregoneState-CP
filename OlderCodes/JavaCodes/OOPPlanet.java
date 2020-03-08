/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// OOPPlanet.java: Planet orbiting Sun
import ptolemy.plot.*;

public class OOPPlanet {
  double Radius, wplanet, xp, yp;               // Orbit r, omega, Planet coordinates
                                                   
  public OOPPlanet() { Radius = 0.; wplanet = 0.; }            // Default constructor
	 
  public OOPPlanet(double Rad, double pomg) { Radius = Rad; wplanet = pomg; }
		
  public double getX( double time )                      // Get x of planet at time t
  { return Radius*Math.cos( wplanet*time ); }
	
  public double getY( double time )                  // Get y of the planet at time t
  { return Radius*Math.sin( wplanet*time ); }
	
  public void trajectory() {                              // Trajectory of the planet
    double time;
    Plot myPlot = new Plot();
    myPlot.setTitle( "Motion of a planet around the Sun" );
    myPlot.setXLabel(" x");
    myPlot.setYLabel(" y");
    for(time = 0.;time <3.2; time = time + 0.02)  {
      xp = getX( time );
      yp = getY( time );
      myPlot.addPoint(0, xp, yp, true);  }    
    PlotApplication app = new  PlotApplication( myPlot );
    }
}                                                               // Planet class ends

class OOPMoon extends OOPPlanet {               // OOPMoon = daughter class of planet
  double radius, wmoon, xm, ym;                   // R, omega satellite, moon wrt Sun

  public OOPMoon()  { radius = 0.;   wmoon = 0.; } 
	 
  public OOPMoon(double Rad, double pomg, double rad, double momg) {   // Full Constr
   Radius = Rad;   wplanet = pomg;  radius = rad;  wmoon = momg; }
	                                             // Coordinates of moon relative to Sun
  public void trajectory() {
    double time;
    Plot myPlot = new Plot();
    myPlot.setTitle("Satellite orbit about planet");
    myPlot.setXLabel(" x");
    myPlot.setYLabel(" y");
    for(time = 0.; time<3.2; time = time+0.02){
      xm = getX(time) + radius*Math.cos(wmoon*time);
      ym = getY(time) + radius*Math.sin(wmoon*time);
      myPlot.addPoint(0, xm, ym, true); }
    PlotApplication app = new  PlotApplication(myPlot);
    }
		
  public static void main(String[] argv) { 
    double Rad, pomg, rad,  momg;
    Rad = 4.;                                                               // Planet
    pomg = 2.;                                          // Angular velocity of planet
    rad = 1.;                                               // Satellite orbit radius
    momg = 14.;                                  // Ang. vel, satellite around planet
    // Uncomment next 2 lines for planet trajectory and
//      comment other two (Moon) lines
    //OOPlanet earth = new OOPlanet(Rad, pomg);
    //earth.trajectory();
    //next two lines if desirethe Moon trajectory
    //but previous two lines must be commented
    OOPMoon Selene = new OOPMoon(Rad, pomg, rad, momg);
    Selene.trajectory();
}}
