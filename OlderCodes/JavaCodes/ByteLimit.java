/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC Bordeianu 
   Copyright Princeton University Press, Princeton, 2007.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2007;
   MJ Paez, Univ Antioquia, 2007; & CC Bordeianu, Univ Bucharest, 2007
   Support by National Science Foundation                              
*   
*********************************************************
* ByteLimit.java: Find largest value allowed by byte data type.
* (crude program, need to pick thru lots of output
*/ 
public class ByteLimit 
{
  public static void main(String[] args) 
  {
    final int N = 256;                                     //Declaration
    int i;                                                        //Declaration 
    byte eps=0;                                             //Declaration
    for (i = 1; i<=N; i=i+1) 
    {
      eps = (byte)(eps + 1);                          //must cast result back from int to byte
      System.out.println("Step #  " + i  + "   gives byte value  " + eps); 
    }
  }
}

/*
OUTPUT
Step #  1   gives byte value  1
Step #  2   gives byte value  2
Step #  127   gives byte value  127
Step #  128   gives byte value  -128
Step #  129   gives byte value  -127
Step #  255   gives byte value  -1
Step #  256   gives byte value  0
*/
