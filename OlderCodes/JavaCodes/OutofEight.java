/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
		 by RH Landau, MJ Paez, and CC BORDEIANU 
		 Copyright Princeton University Press, Princeton, 2008.
		 Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
		 MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
		 Support by National Science Foundation																													 
		 */
// Game of the life with 1 out of 8 rule 
import java.io.*;
public class OutofEight	 {	
		
  public static void main (String[] argv)throws IOException		 {
	  int cell[][][]=new int[34][34][2],i, r, j, alive;
				for ( j = 0; j < 33; j++ )	{						            // Initial state of cells
					for ( i = 0; i < 33; i++ ) cell[j][i][0] = 0;			
					cell[16][16][0] = 1;
					for (i = 0; i < 33; i++) System.out.print(" "+cell[j][i][0]);
					System.out.println("");
				}																											                 //	 j
				for ( r = 0; r < 10; r++ ) {
					for ( j = 1; j < 32; j++ ) {
						for ( i = 1; i < 32; i++ ) {
							alive=0;
							if (	 cell[i-1][j][0] == 1 ) alive = alive + 1;
							if (	 cell[i+1][j][0] == 1 ) alive = alive + 1;
							if (	 cell[i][j-1][0] == 1 ) alive = alive + 1;
							if (	 cell[i][j+1][0] == 1 ) alive = alive + 1;
							if ( cell[i-1][j-1][0] == 1 ) alive = alive + 1;
							if ( cell[i+1][j-1][0] == 1 ) alive = alive + 1;
							if ( cell[i-1][j+1][0] == 1 ) alive = alive + 1;
							if ( cell[i+1][j+1][0] == 1 ) alive = alive + 1;
							if ( cell[i][j][0] == 0 && alive == 1 ) cell[i][j][1]=1;
						}																												             // i
				}																														             // j
				for(j=0;j<33;j++) {for(i=0;i<33;i++)cell[j][i][0]=cell[j][i][1];}
				System.out.println("Press any key to continue ");
				for ( j = 0; j < 33; j++ )	{
					for ( i = 0; i<33; i++ )	{
					if ( cell[j][i][1] == 1)	System.out.print("X");
						else System.out.print(" ");
					}
					System.out.println("");
				}																														             // j
				System.in.read();
				}																														             // r
} }					 