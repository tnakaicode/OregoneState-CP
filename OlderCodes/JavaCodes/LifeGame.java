/** The Game of life- Cellular automata in 2 dimensions
  * Rules: a cell can be either dead (0) or alive (1)
  * If a cell is alive:
  * on next step will remain alive if
  * 2 or 3 of its closer 8 neighbors are alive.
  * If more than 3 of its 8 neighbors are alive the cell dies of overcrowdedness.
  * If less than  2 neighbors are alive the cell dies of loneliness
  * A dead cell will be alive if more than 3 of its 8 neighbors are alive.
 */ 
public class LifeGame{  
  public static void main (String[] argv){
    int cell[][][] = new int[30][30][2], i, r, j, alive;
    for ( j = 0; j < 30; j++ ) {                        // initial state of all cells
      for ( i = 0; i < 30; i++ ) {  
        r=(int)(Math.random()*2);
        cell[j][i][0] = r;                                 // dead or alive at random
      }
      for( i = 0; i < 30; i++) System.out.print(" " + cell[j][i][0]);
      System.out.println("");
    }                                                                        // for j
    for ( j = 1; j < 29; j++) {
      for ( i = 1;i<29; i++) {
        alive = 0;
        if (cell[i-1][j][0]   == 1) alive=alive+1;                  // check neighbors
        if (cell[i+1][j][0]   == 1) alive=alive+1;
        if (cell[i][j-1][0]   == 1) alive=alive+1;
        if (cell[i][j+1][0]   == 1) alive=alive+1;
        if (cell[i-1][j-1][0] == 1) alive=alive+1;
        if (cell[i+1][j-1][0] == 1) alive=alive+1;
        if (cell[i-1][j+1][0] == 1) alive=alive+1;
        if (cell[i+1][j+1][0] == 1) alive=alive+1;
        if (cell[i][j][0] == 1) {
          if ( alive == 2 || alive == 3 ) cell[i][j][1] = 1;
          if ( alive > 3 )                cell[i][j][1] = 0;
          if ( alive < 2 )                cell[i][j][1] = 0;
        }                                                               // cell alive
        if ( cell[i][j][0] == 0  &&  alive > 3 ) cell[i][j][1]=1;
      }                                                                      // for i
    }                                                                        // for j
    System.out.println("Next situation ");
    for ( j = 0; j < 30; j++ ) {
      for ( i = 0; i < 30; i++ )  System.out.print(" "+cell[j][i][1]);
      System.out.println("");
} } }                                                                 
 