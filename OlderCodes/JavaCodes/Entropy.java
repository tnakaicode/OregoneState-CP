/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
// Entropy.java, Shannon entropy of logistic map   
import java.io.*; 
import java.util.*; 

public class Entropy  {

  public static void main(String[] argv) throws IOException, FileNotFoundException {
    PrintWriter w = new PrintWriter (new FileOutputStream("Entropy.dat"), true); 
    double prob[] = new double[1001];                                 // Probabilites
    int nbin = 1000, nmax = 100000, j, n, ibin; 
    double entropy, x, mu; 
    System.out.println("Entropy output in Entropy.dat"); 
    for ( mu = 3.5; mu <= 4. ; mu = mu + 0.001)  {                   // Values of  mu
      for ( j=1; j < nbin; j++ ) prob[j] = 0;
      x = 0.5; 
      for ( n=1; n <= nmax; n++ )  {  
        x = mu*x*(1.-x);                             // Logistic map, Skip transients
        if (n > 30000) { ibin = (int)(x*nbin) + 1;  prob[ibin] = prob[ibin] + 1;}
      }
      entropy = 0. ; 
      for ( ibin = 1; ibin <= nbin; ibin++ ) if (prob[ibin]>0) 
           entropy = entropy-(prob[ibin]/nmax)*Math.log(prob[ibin]/nmax);
      w.println(" " + mu + " " + entropy); 
    }
  }
} 