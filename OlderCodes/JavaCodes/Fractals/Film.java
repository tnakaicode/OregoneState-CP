/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; and CC BORDEIANU, Univ Bucharest, 2007.
   Support by National Science Foundation                              
   */
// Film_1.java: if command line argument print h(t); else coastline
import java.util.Random;

public class Film {
    
  public static void main(String[] args) {
    boolean printavg = args.length > 0;
    int[] coast = new int[200];
    Random random = new Random();
    for (int i = 0; i < coast.length; i++) coast[i] = 0;
    for (int i = 0; i < 20000; i++) {                  // Number of particles dropped
      int spot = random.nextInt(200);
      if (spot == 0) {                        // Hitting edges counts as filling hole
        if ( coast[spot] < coast[spot+1] ) coast[spot] = coast[spot+1];
        else coast[spot]++;
      }
      else if (spot == coast.length - 1) {
        if (coast[spot] < coast[spot-1]) coast[spot] = coast[spot-1];
        else coast[spot]++;
      }
      else if ((coast[spot]<coast[spot-1]) && (coast[spot]<coast[spot+1])) {
        if (coast[spot-1] > coast[spot+1]) coast[spot] =coast[spot-1];
        else coast[spot] = coast[spot+1];
      }
      else coast[spot]++;
      if (printavg) {                                     // Print the average height
        int sum = 0;
        for (int j = 0; j < coast.length; j++) sum += coast[j]; 
        System.out.println(sum*1.0/coast.length);
      } 
    }
    if (!printavg)
    for (int i = 0; i < coast.length; i++)
      System.out.println("i, avergage height = "+ i + ", " + coast[i]);
  }
}
