/* 
************************************************************************
*  MyRandom.java  A simple random number generator, not for serious work *
*                                                                      *
/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*/                         
//---------------------------------------------------------------------*
//                              !!!WARNING!!!                          *
//  This is an  example of a bad random number generator               *
//---------------------------------------------------------------------* 

import java.io.*;       //Location of PrintWriter

public class MyRandom      

{
  public static void main(String[] argv) throws IOException, FileNotFoundException
{
  //
  //Initialize file to write data to:
  //
  PrintWriter q = new PrintWriter(
       new FileOutputStream("BADRAND.DAT"),true);
  //
  //
  // To write to file:
  //      q.println("Text to write");
  //
  //

  //variable declarations
  int i=0,old=0,newx=0,newy=0;
  int max=10000;                         //number of numbers generated
  int seed=11;                          //seed for random number generator

  old=seed;                             //the seed
  
  for(i=0;i<max;i++)                    //generating #max numbers
    {                             
    newx=(57*old+1)%256;          //x-coordinate
    newy=(57*newx+1)%256;         //y-coordinate
    q.println(newx+" , "+newy);
    old=newy;
    }

  System.out.println(" ");
  System.out.println("MyRandom Program Complete.");
  System.out.println("Data stored in BADRAND.DAT");
  System.out.println(" ");
}


}   // End of file
