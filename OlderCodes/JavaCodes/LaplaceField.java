/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*
*  LaplaceField.java: Solution of Laplace equation with finite differences    *
*                  
*  comment: Output data is saved in 3D grid format used by gnuplot     *
************************************************************************
*/                      
import java.io.*;

public class LaplaceField
 { static int  max= 100;       
public static void main(String[] argv) throws IOException,
FileNotFoundException
{   
   
   double p[][]=new double[max][max];
   double ex[][]=new double[max+1][max+1];
   double ey[][]=new double[max+1][max+1];
   double enorm[][]=new double[max+1][max+1];
   int i, j, iter;
   PrintWriter w = 
        new PrintWriter(new FileOutputStream("Laplace_field1.dat"), true);
        PrintWriter q = 
        new PrintWriter(new FileOutputStream("Laplace_pot1.dat"), true);
            // save data in LaplaceRHL.dat 
  
   
   for(i=0; i<max; i++)                 /* initialize array  */
  {   
      for (j=0; j<max; j++) p[i][j] = 0.0;
   }

    // boundary conditions
    for (i=25; i<=75; i++){
             p[i][37] = -100.0;             
             p[i][63] = 100.0;  }
    for (i=0; i< max; i++){
             p[0][i] = 0.0;
             p[max-1][i] = 0.0; 
             p[i][0] = 0.0;
             p[i][max-1] = 0.0;  
                           } 

 
   for(iter=0;iter<1000;iter++){
  
   for(i=1; i<=max-2; i++)                  // x-direction */
      {
         for(j=1; j<=max-2; j++)               // y-direction */
         {
            //Jacobi's four points discretization
            
            p[i][j] = 0.25*(p[i+1][j]+p[i-1][j]+p[i][j+1]+p[i][j-1]);
        // boundary conditions again
        if((j==37)&&(i>=25)&&(i<=75)) p[i][j]=100.0;
        if((j==63)&&(i>=25)&&(i<=75)) p[i][j]=-100.0 ;
         ex[i][j] = -(p[i+1][j] - p[i-1][j]) ;// compute electric field
         ey[i][j] = -(p[i][j+1] - p[i][j-1]); // components
         enorm[i][j] =Math.sqrt(ex[i][j] *ex[i][j] +ey[i][j] *ey[i][j] );
         // norm 
         
         
         }
      }
  }
   for (i=1; i<max ; i=i+5)         // write data gnuplot 3D format 
   {
      for (j=1; j<max; j=j+5) 
      {  
         q.println(" "+p[j][i]+" ");
         w.println(" "+i/5+" "+j/5+" "+ex[i][j]/enorm[i][j] +" "+ey[i][j]/enorm[i][j] +" ");
      }
     q.println(" ");       
   }
   System.out.println("data stored in Laplace_field1.dat");
  
}



}
