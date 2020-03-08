/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Komplex: Cartesian/polar complex via interface
// 'type = 0' -> polar representation, else rectangular

public class Komplex implements KomplexInterface   {    
  public double mod, theta, re, im; 
  
  public Komplex()  {                                          // Default constructor
    mod = 0;  theta = 0;    re = 0;       im = 0;  }
  
  public Komplex(double x, double y, int type)  {                       //Constructor
    if (type == 0)  {mod = x;  theta = y; }  else {re = x;   im = y; }  }
  
  public double getRe() { return mod*Math.cos(theta); }
    
  public double getIm() { return mod*Math.sin(theta); }
    
  public double setRe() { re = mod*Math.cos(theta); return re; }
      
  public double setIm() { im = mod*Math.sin(theta); return im; }
      
  public void add(Komplex other, int type)  { 
    double tempMod = 0.; 
    if (type == 0)  { 
      tempMod = Math.sqrt(this.mod*this.mod + other.mod*other.mod 
            +   2*this.mod*other.mod*Math.cos(this.theta-other.theta));
      this.theta = Math.atan2(this.mod*Math.sin(this.theta) 
        + other.mod*Math.sin(other.theta), this.mod*Math.cos(this.theta)
                                     + other.mod*Math.cos(other.theta));
      this.mod = tempMod; 
    } else { this.re = this.re + other.re; this.im = this.im + other.im; }
  }
  
  public void sub(Komplex other, int type) {
    if (type == 0)  { 
      this.mod = Math.sqrt(this.mod*this.mod +  other.mod*other.mod -
       2*this.mod*other.mod*(Math.cos(this.theta)*Math.cos(other.theta)
                       + Math.sin(this.theta)*Math.sin(other.theta))); 
      this.theta =  Math.atan((this.mod*Math.sin(this.theta)
        -other.mod*Math.sin(other.theta))/(this.mod*Math.cos(this.theta)
                                    -other.mod*Math.cos(other.theta))); 
    } else { this.re = this.re-other.re; this.im = this.im-other.im; } 
  }
  
   public void div(Komplex other, int type)  { 
    if (type == 0) { this.mod = this.mod/other.mod; 
                     this.theta = this.theta-other.theta; 
    } else{ this.re = (this.re*other.re + this.im*other.im)/
         (other.re*other.re + other.im*other.im); 
          this.im = (this.im*other.re-this.re*other.im)/
         ( other.re*other.re +  other.im*other.im ); 
      } 
  }
   
  public void mult(Komplex other, int type)  {
    if (type == 0)  {
      this.mod = this.mod*other.mod; 
      this.theta = this.theta + other.theta; 
    } else { 
      Komplex ans = new Komplex(); 
      ans.re = this.re*other.re-this.im*other.im; 
      ans.im = this.re*other.im + this.im*other.re; 
      this.re = ans.re; 
      this.im = ans.im;     
      }
  }
      
  public void conj(int type)  {
    if (type == 0)  { this.mod = this.mod; this.theta = -this.theta; }
    else  {this.re = this.re; this.im = -this.im; }   } 
}
         
