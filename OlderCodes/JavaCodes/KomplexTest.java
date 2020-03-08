/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//   KomplexTest:   test KomplexInterface
public class KomplexTest  {
  
  public static void main(String[] argv)  {
    Komplex a, e; 
    e = new Komplex(); 
    a = new Komplex(1., 1., 1); 
    Komplex b = new Komplex(1., 2., 1); 
    System.out.println  ("Cartesian: Re a = " + a.re + ", Im a = " + a.im + ""); 
    System.out.println  ("Cartesian: Re b = " + b.re + ", Im b = " + b.im + ""); 
    b.add(a, 1); 
    e = b; 
    System.out.println("Cartesian: e=b + a=" + e.re + " " + e.im + ""); 
     // Polar Version, uses get and set methods
    a = new Komplex(Math.sqrt(2.), Math.PI/4., 0);          // Polar via 0
    b = new Komplex(Math.sqrt(5.), Math.atan2(2., 1.), 0); 
    System.out.println ("Polar: Re a = " + a.getRe() + ", Im a = " + a.getIm() + ""); 
    System.out.println ("Polar: Re b = " + b.getRe() + ", Im b = " + b.getIm() + ""); 
    b.add(a, 0); 
    e = b; 
    System.out.println ("Polar e=b + a = " + e.getRe() + " " + e.getIm() + ""); 
  } 
}
