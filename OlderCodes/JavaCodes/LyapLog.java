/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
 // LyapLog.java: Lyapunov coef for logistic map    
import java.io.*; 

public class LyapLog {     
  static double  m_min = 2.8, m_max = 4., step = 0.002 ;
  
  public static void main(String[] argv) throws IOException, FileNotFoundException {
    double  m, y, suma, lyap[] = new double[100]; 
    int i; 
    PrintWriter w =  new PrintWriter(new FileOutputStream("logistic.dat"), true);
    PrintWriter q =  new PrintWriter(new FileOutputStream("lyap.dat"), true);
    System.out.println("data stored in logistic.dat and lyap.dat");
    for ( m = m_min;   m <= m_max;  m += step)  {                           // m loop
      y = 0.5;
      for ( i=1;  i <= 200;  i++ )  y = m*y*(1-y); 
      suma = 0. ;                                                  // Skip transients
      for ( i=201;  i <= 401;  i++ )  { 
        y = m*y*(1-y); 
        suma = suma + Math.log(Math.abs(m*(1.-2.*y)));                    // Lyapunov
        w.println(  m + "  " + y); 
      }
      lyap[(int)m] = suma/401;                         // Normalize Lyapunov exponent
      q.println( m + "    " + lyap[(int)m]) ;   
} } } 