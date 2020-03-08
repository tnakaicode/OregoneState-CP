/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
	 by RH Landau, MJ Paez, and CC BORDEIANU 
	 Copyright Princeton University Press, Princeton, 2008.
	 Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
	 MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
	 Support by National Science Foundation															 
	 */
/* A cellular automaton following the rules: 									
                  old line:			 0 0		1 1		1 0		 0 1
									new line				 0			0			1			 1							            */
									
public class CellAut {	

	public static void main (String[] argv) {
		int data[][] = new int[30][2]; int i, r, j;
		for ( i = 0; i < 30; i++ )	{														            // First line
			data[i][0] = (int)(Math.random()*2);
			System.out.print(" "+data[i][0]);
		}
		for( j = 0; j < 30; j++)	{															             // New lines
			System.out.println("");
			data[0][1] = (int)(Math.random()*2);						             // 1st cell random
			System.out.print( " "+data[0][1] );
			for ( i=1; i<30; i++)	 {											             // Rules are applied
				if ( data[i-1][0]==0 && data[i][0]==0 ) data[i][1]=0;
				if ( data[i-1][0]==1 && data[i][0]==1 ) data[i][1]=0;
				if ( data[i-1][0]==0 && data[i][0]==1 ) data[i][1]=1;
				if ( data[i-1][0]==1 && data[i][0]==0 ) data[i][1]=1;
				System.out.print(" "+data[i][1]);
			}																					
			for ( i=0; i<30; i++ ) data[i][0]=data[i][1];	               // New data to old
} } } 