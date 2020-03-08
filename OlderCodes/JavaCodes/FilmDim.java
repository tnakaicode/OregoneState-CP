/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/  
// Random Coastline (film) Generator with fractal dimension

import java.io.*;
import java.util.*;

public class FilmDim
{
   public static void main(String[] argv) throws IOException, FileNotFoundException
   {
   PrintWriter deposit = new PrintWriter(new FileOutputStream("deposit.dat"),true);
   PrintWriter dep = new PrintWriter(new FileOutputStream("dep.dat"),true);
   PrintWriter dArr1 = new PrintWriter(new FileOutputStream("dArray1.dat"),true);
   PrintWriter dArr2 = new PrintWriter(new FileOutputStream("dArray2.dat"),true);
   long seed = 8994342;
   Random randnum = new Random(seed);
   int max = 30000;
   boolean test=true;
   int i, j, k, r;
   int [] h =  new int [1000];
   int [][] depArray = new int [1000][1000];
   for (i=0; i<1000; i++)
   {
      for(j=0; j<1000; j++) {depArray [i][j] = 0;}
   }
   
   for (i=0; i<1000; i++) {h[i] = 0;}
   for (i=1; i<max; i++)
   {  
     r = (int)(998*randnum.nextDouble())+1; 
     if ((h[r] >= h[r-1]) && (h[r] >= h[r+1])) {h[r]++;}
     else if (h[r-1] > h[r+1]) {h[r] = h[r-1];}
     else {h[r]=h[r+1];}
     deposit.println(r+" "+h[r]);
     depArray[r][999-h[r]] = 1;
   }
   for (i=0; i<1000; i++)
   {
      for (j=0; j<999; j++)
      {
         if ((depArray[i][j] == 1 ) && test) {test = false;}
         else depArray[i][j] = 0;
      }
      test = true;
   }
   
   PrintWriter dres = new PrintWriter(new FileOutputStream("depres.dat"),true);
   int size= 1000;
   int m=0,n=0;
   boolean box = false;
   double d1, d2, d3, davg;
   int [] boxcount = new int[4];
   int [] Res = new int[4]; 
      for (i=0; i<4;i++) {boxcount[i]=0;}
      Res[0] = 50; Res[1] = 25; Res[2] = 10; Res[3] = 5;      
      for (k=0; k<4; k++)
      {
      for (m=0; m+Res[k]<size; m+=Res[k])
      {
      for (n=0; n+Res[k]<size; n+=Res[k])
      {
         int [][] submap = new int[Res[k]][Res[k]];   //Set subarray of coastmap
         for (i=0; i<Res[k]; i++)         //Load subarray
         {
            for (j=0; j<Res[k]; j++)
            {
               submap [j][i] = depArray[n+j][m+i];
            }
         }
         for (i=0; i<Res[k]; i++)         //Analyze subarray
         {
            for (j=0; j<Res[k]; j++)
            {
               if (submap[i][j]==1) {box = true;};
            }
         }
         if (box) {boxcount[k]++;}           // count boxes
         box = false;
      }
      }
      }
      for (i=0; i<4; i++) {
      System.out.println(boxcount[i]);
      dres.println(Math.log(1./Res[i])+" "+Math.log(boxcount[i]));
      
      }
      double bc1 = (double) boxcount[0];
      double bc2 = (double) boxcount[1];
      double bc3 = (double) boxcount[2];
      double bc4 = (double) boxcount[3];
      double R1 = (double) Res[0];
      double R2 = (double) Res[1];
      double R3 = (double) Res[2];
      double R4 = (double) Res[3];
      
      d1 = Math.abs(Math.log(bc2/bc1)/Math.log(R2/R1));
      d2 = Math.abs(Math.log(bc3/bc2)/Math.log(R3/R2));
      d3 = Math.abs(Math.log(bc4/bc3)/Math.log(R4/R3));
      davg = (d1+d2+d3)/3.;
      System.out.println(davg);            
        
   }   

} 