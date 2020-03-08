/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008
   Support by National Science Foundation                              
   */
// Wang Landau algorithm for 2-D spin system
// Author: Oscar A. Restrepo, Universidad de Antioquia, Medellin, Colombia

import java.io.*;

public class WangLandau {
  public static int L = 8, N =(L*L), sp[][] = new int[L][L];      // Grid size, spins
  public static int hist[] = new int[N+1], prhist[] = new int[N+1];     // Histograms
  public static double S[]      = new double[N+1];              // Entropy = log g(E)
  public static int iE(int e)   { return (e+2*N)/4; }
    
  public static void WL()  {
    double fac, Hinf=1.e10, Hsup=0., deltaS, tol=1.e-8;      // f, hist, entropy, tol
    double height, ave, percent;                    // Hist height, avg hist, % < 20%
    int i, xg, yg, j, iter, Eold, Enew;                   // Grid positions, energies
    int ip[] = new int[L], im[] = new int[L];                // BC R or down, L or up
    height = Math.abs(Hsup-Hinf)/2.;                          // Initialize histogram
    ave = (Hsup+Hinf) / 2.;                             // about average of histogram
    percent = height / ave;  
    for (i=0; i<L; i++) for(j=0; j<L; j++) sp[i][j] = 1;             // Initial spins
    for (i=0; i<L; i++)  { ip[i] = i+1;   im[i] = i-1; }          // Case plus, mimus
    ip[L-1] = 0;  im[0] = L-1;                                             // Borders
    Eold = -2*N;                                                 // Initialize energy
    for ( j = 0; j<=N; j++ ) S[j] = 0;                         // Entropy initialized
    iter = 0; fac = 1;                                         // Initial factor ln e
    while ( fac > tol )  { 
      iter++;
      i = (int)(Math.random()*N);                               // Select random spin
      xg = i%L;      yg = i/L;                                 // Localize grid point
      Enew = Eold + 2*( sp[ip[xg]][yg] + sp[im[xg]][yg] + sp[xg][ip[yg]] 
             + sp[xg][im[yg]] ) * sp[xg][yg];                        // Change energy
      deltaS = S[iE(Enew)] - S[iE(Eold)];                           // Change entropy
      if ( deltaS <= 0 || Math.random() < Math.exp(-deltaS) ) { Eold = Enew; 
                                                   sp[xg][yg] *= -1; }   // Flip spin
      hist[iE(Eold)]++;                            // Change histogram, add 1, update
      S[iE(Eold)] += fac;                                           // Change entropy
      if( iter%10000 == 0) {                     // Check flatness every 10000 sweeps
        for(j = 0; j <= N; j++)  {
          if ( j == 0 )  { Hsup = 0;   Hinf = 1e10; }     // Initialize new histogram
          if ( hist[j] == 0 )  continue;                    // Energies never visited
          if ( hist[j] > Hsup ) Hsup = hist[j];
          if ( hist[j] < Hinf ) Hinf = hist[j];
        }
        height = Hsup-Hinf; 
        ave = Hsup+Hinf;
        percent = height/ave;
        if( percent < 0.2 )  {                                     // Histogram flat?
          System.out.println(" iter "+iter +"   log(f) "+fac );
          for( j=0; j<=N; j++ )  {prhist[j] = hist[j];  hist[j] = 0;}    // Save hist
          fac *= 0.5;                                   // Equivalent to log(sqrt(f))
}  }  }  }

  public static void IntEnergy()throws IOException, FileNotFoundException  {
    double T, maxL, Ener, U, sumdeno, sumnume, exponent = 0;      // Temp, max lambda
    int i;  
    PrintWriter b  = new PrintWriter(new FileOutputStream ("IntEnergy.dat"), true);
    for( T = 0.2; T <= 8.0; T += 0.2 ) {                         // Select lambda max
      Ener = -2*N;
      maxL = 0.0;                                                       // Initialize
      for( i=0; i<N; i++ ) { if (S[i]!=0 && (S[i]-Ener/T)>maxL) maxL = S[i]-Ener/T;
                              Ener = Ener+4; }
      sumdeno = sumnume = 0;
      Ener = -2*N;
      for( i=0; i<N; i++ ){
        if( S[i] != 0) exponent = S[i]-Ener/T-maxL;
        sumnume += Ener*Math.exp(exponent);
        sumdeno += Math.exp(exponent);
        Ener = Ener+ 4.0;
      }
      U = sumnume/sumdeno/N;
      b.println("  "+T+"   "+U);
      System.out.println("Output data in IntEnergy.dat");
    }
  }

  public static void  main(String[] argv)throws IOException, FileNotFoundException  {
    PrintWriter q  = new PrintWriter( new FileOutputStream("wanglandau.dat"), true );
    int order, j ;
    double deltaS = 0.0;   
    WL();                                               // Call Wang Landau algorithm
    for( j=0; j <= N; j++ )  {
     order = j*4 - 2*N;
     deltaS = S[j] - S[0] + Math.log(2);
     if( S[j] != 0 ) q.println("   "+ 1.*order/N +"   "  + deltaS+"  "+ prhist[j]);
   }    
   IntEnergy();
   System.out.println("Output data in wanglandau.dat");
}  }                                                                   // Main, class