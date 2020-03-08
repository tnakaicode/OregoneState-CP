/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//  ComplexDyn.java:  Complex object class with non-static members 

public class ComplexDyn { 
  public double re; public double im;                    // Nonstatic class variables
   
  public ComplexDyn() { re = 0; im = 0; }                      // Default constructor
   
  public ComplexDyn(double x, double y) { re = x; im = y; }            // Constructor
   
  public void add(ComplexDyn other)                           // Dynamic other + this
    { this.re = this.re + other.re;  this.im = this.im + other.im; }
      
  public void mult(ComplexDyn other) {                          // Dynamic other*this
    ComplexDyn ans = new ComplexDyn();                                // Intermediate
    ans.re  = this.re * other.re - this.im * other.im; 
    ans.im  = this.re * other.im + this.im * other.re; 
    this.re = ans.re;                              // Copy value into returned object
    this.im = ans.im;
  }
    
  public static void main(String[] argv) {                // Indep static Main object
    ComplexDyn a, b;                                     // Declare 2 Complex objects
    a = new ComplexDyn();                                           // Create objects
    b = new ComplexDyn(4.7, 3.2); 
    ComplexDyn c = new ComplexDyn(3.1, 2.4);                       // Declare, create
    System.out.println("a,b=("+a.re+", "+a.im+"),("+b.re+", "+ b.im+"),");
    System.out.println("c = (" +c.re+ ", " +c.im+ ")"); 
    c.add(b);                                                       // Non-static add
    a = c; 
    System.out.println("b + c = (" + a.re + ", " + a.im + "), "); 
    c = new ComplexDyn(3.1, 2.4); 
    c.mult(b);                                                     // Non-static mult
    System.out.println("b*c = (" + c.re + ", " + c.im + ")"); 
  }
}
