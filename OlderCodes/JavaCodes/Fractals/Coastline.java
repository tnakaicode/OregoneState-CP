/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */     
import java.util.Random;
import java.io.*;
// Coastline.java: if command line argument, print average height (t); else coastline

public class Coastline{
    
  public static void main(String[] args)throws IOException, FileNotFoundException {
    PrintWriter q  = new PrintWriter(new FileOutputStream("coastline.dat"), true);
    boolean printavg = args.length > 0;
    int[] coast = new int[200];
    int i;
    Random random = new Random();
    for ( i = 0; i < coast.length; i++) coast[i] = 0;
    for ( i = 0; i < 20000; i++) {                           // All particles dropped
      int spot = random.nextInt(200);
      if (spot == 0) {                         // Hitting edge counts as filling hole
        if (coast[spot] < coast[spot+1]) coast[spot] = coast[spot+1];
        else coast[spot]++;
       }
       else if (spot == coast.length - 1) {
         if (coast[spot] < coast[spot-1]) coast[spot] = coast[spot-1];
         else coast[spot]++;
       }
       else if ((coast[spot]<coast[spot-1]) && (coast[spot]<coast[spot+1])) {
         if (coast[spot-1] > coast[spot+1]) coast[spot] = coast[spot-1];
         else coast[spot] = coast[spot+1];
       }
       else coast[spot]++;
       if (printavg) {                                        // Print average height
         int sum = 0;
         for (int j = 0; j < coast.length; j++) sum += coast[j];
         System.out.println(sum*1./coast.length);
       }
    }
    if (!printavg) for (i = 0; i < coast.length; i++) {
      System.out.println(coast[i]);
      q.println(" "+i+" "+coast[i]+" ");
		}
  }
}
