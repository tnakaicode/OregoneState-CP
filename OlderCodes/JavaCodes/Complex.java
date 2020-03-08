/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Complex.java:   Creates "Complex" class with static members

public class Complex {
  public double re, im;                                  // Nonstatic class variables
   
  public Complex ()  { re = 0;  im = 0; }                      // Default constructor
   
  public Complex(double x, double y)  { re = x;  im = y; }        // Full constructor
   
  public static Complex add(Complex a, Complex b) {              // Static add method
    Complex temp = new Complex();                              // Create Complex temp
    temp.re = a.re + b.re; 
    temp.im = a.im + b.im; 
    return temp; 
  }
      
  public static Complex mult(Complex a, Complex b) {            // Static mult method
    Complex temp = new Complex();                              // Create Complex temp
    temp.re = a.re * b.re - a.im * b.im; 
    temp.im = a.re * b.im + a.im * b.re; 
    return temp;
  }
      
  public static void main(String[] argv) {                             // Main method
    Complex a, b;                                        // Declare 2 Complex objects
    a = new Complex();                                          // Create the objects
    b = new Complex(4.7, 3.2); 
    Complex c = new Complex(3.1, 2.4);                        // Declare, create in 1
    System.out.println("a,b=("+a.re+","+a.im+"), ("+b.re+","+b.im+"),");
    System.out.println("c = ("+c.re+ ", " +c.im+ ")"); 
    a = add(b, c);                                              // Perform arithmetic
    System.out.println( "b + c = (" +a.re+ ", " +a.im+ "),  "); 
    a = mult(b, c); 
    System.out.println( "b*c = (" +a.re+ ", " +a.im+ ")"); 
  }
}
