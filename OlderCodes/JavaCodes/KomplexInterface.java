/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
//        KomplexInterface:   complex numbers via interface

public interface KomplexInterface {   
    public double getRe(); 
    public double getIm(); 
    public double setRe(); 
    public double setIm(); 
     // type = 0: polar representation;  other: rectangular
    public void add(Komplex  other, int type); 
    public void sub(Komplex other, int type); 
    public void mult(Komplex other, int type); 
    public void div(Komplex other, int type); 
    public void conj(int type);         }
