/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// QMCbouncer.java: quantum bouncer wavefunction via path integration
//  Author: Oscar Restrepo, Universidad de Antioquia 
import java.io.*;
import java.util.*;

public class QMCbouncer{   
  static int max = 300000, N = 100;                            // Trajectories, array
  static double dt = 0.05, g  =  2.0;                           // Time step, gravity
  
  public static void main(String[] argv) throws IOException, FileNotFoundException  {
        PrintWriter q  = new PrintWriter(new FileOutputStream("bouncer.dat"), true);
    double change, newE, oldE, norm, h, firstlast, maxx, path[] = new double[101];                                 
    int prop[] = new int[201], i, j, element, ele, maxel;            // Probabilities
    h=0.00;
    for ( j = 0; j <= N; j++ )  {path[j]=0.0; prop[j]=0;}               // Initialize
    oldE = energy(path);                                                 // Initial E
    maxel = 0;                     
    for (i = 0; i < max; i++ ) {
      element = (int)(Math.random()*N);
      if( element!=0 && element!= N )  {                          // Ends not allowed
        change  = ( (Math.random()-0.5)*20. )/10.;                          //-1 to 1
        if ( path[element] + change > 0.)  {                     // No negative paths
          path[element] += change;
          newE = energy(path);                                    // New trajectory E
          if (newE>oldE && Math.exp(-newE+oldE) <= Math.random() )   
            { path[element] -= change; }                             // Link rejected
          ele = (int) (path[element]*1250./100.);                    // Scale changed
          if ( ele >= maxel ) maxel=ele;                       // Scale change 0 to N
          if ( element !=0 )  prop[ele]++;
          oldE = newE;                                              // For next cicle
    } } }                        
    maxx=0.0;
    for ( i = 1; i < N; i++ ) if ( path[i] >= maxx) maxx=path[i];             // Norm
    norm = 0.;                         
    h = maxx/maxel;
    firstlast = h*0.5* ( prop[0] + prop[maxel] );
    for ( i=1; i <= maxel; i++ ) norm = norm+prop[i];                    // Trap rule
    norm = norm*h + firstlast;
    for (i=0; i<=maxel; i++) q.println(" "+h*i+" "+(double)(prop[i])/norm);
    System.out.println("data stored in bouncer.dat \n");
  }                                                                           // main
 
  static double energy (double arr[]) {                                  // E of path
    int i;
    double sum=0.;
    for ( i = 0; i < N; i++ )                                               // KE, PE
      {sum += 0.5*Math.pow( (arr[i+1]-arr[i])/dt, 2 )+ g * ( arr[i] + arr[i+1] )/2.;}             //  Linear V
    return (sum);
} }  