/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
   // CWTapplication.java
import java.io.*;
import java.util.*;
import java.awt.image.*;
import java.awt.*;
import java.awt.event.*;

public class CWTapplication extends Frame  {  
	int i,j,k;
	double dtau;
	int c[][]= new int[512+1][512+1];                                       //amplitude
	int d[][]= new int[512+1][512+1];
	double u[]=new double [8192+1];
	double	omega2 = 128.0;                                           //final frequency
	double	omega1 = 4.0;                                           //initial frequency
	double input[] = new double[120000];
  double amplitude[] = new double[512*512];
  int image[] = new int[512*512+1];
  Image mem_img;	
  int input_length=8192;
    	
  public CWTapplication(){
		setSize(440,380);
		addWindowListener(new WindowAdapter()
		{ public void windowClosing(WindowEvent event)
		{ System.exit(0); }});
}

	public void paint(Graphics g) { 
		int i,j,k,o;
		g.setColor(Color.blue); 
		g.drawString("Spectrogram using Morlet wavelet",130,35);	
		try{                                                        //read data from file
			FileInputStream fis = new FileInputStream("armadillo.dat");
			BufferedReader br = new BufferedReader(new InputStreamReader(fis));					
			StreamTokenizer st = new StreamTokenizer(br);
			i=0;
			while (st.nextToken()!= StreamTokenizer.TT_EOF) {
				input[i]=st.nval;
				i++;}
			}
			catch(IOException e)
			{
		System.out.println(" File error:"+e+" ");	
			}		
		double tau, omega, domega, x, WTreal, WTimag;
			// Psi(t)=Psi((t-tau)/s)=Psi((t-tau)*omega)
			// frequency omega=1/s  tau2=tau1+n*dtau      translation
		dtau=1.0/512.0;
		double PsiReal[]= new double[16834];						//real part of wavelet function
		double PsiImag[]= new double[16834];						// imag part of wavelet function
 			//scaling, omega2=omega1*domega^m     frequency omega=1/s
			// domega=(omega2/omega1)^1/m	
		domega= Math.pow(omega2/omega1,1.0/512.0);
		double max = 0.0001;
		omega = omega1;
		for (i=0;i<512;i++) {  
			// compute daughter wavelet function using scaling and translation 
			tau = -8192.0*dtau;
			for (o=0; o<16384; o++) {
				// 16384=2*2^13 = 2*8192
				// the program can handle signal files up to 8192 sample values in length
				PsiReal[o] = WaveletReal( tau*omega );
				PsiImag[o] = WaveletImag( tau*omega );
				tau = tau + dtau; //translation
			}
			for (j=0;j<512;j++) { // compute values of CWT 
				WTreal = 0.0;
				WTimag = 0.0;
				for (o=0;o<input_length;o++) {
					WTreal += input[o]*PsiReal[8192-(j*input_length)/512+o];
					WTimag += input[o]*PsiImag[8192-(j*input_length)/512+o];
				}
				x = Math.sqrt(WTreal*WTreal+WTimag*WTimag);//amplitude
				k = 512*i+j;
				amplitude[k] = x;
				if (max<x) max = x;
				}
				omega = omega*domega;//scaling
       }
			o=0;		// process the image
			for (i=0;i<512;i++) {
				for (j=0;j<512;j++)	{
					k = 512*j+i;
					c[i][j]= 256 - (int)(256.0*amplitude[k]/max);
					if (c[i][j]<0) {c[i][j] = 0;}
					if (c[i][j]>255) {c[i][j]=255;}
			    }
				}	   
				for (i=0;i<512;i++) { for (j=0;j<512;j++) {
					d[i][j]=c[j][512-i];
					image[o]=d[i][j]<<24;
					o++;
					}	}	   
			 mem_img=createImage(new MemoryImageSource(512,512,image,0,512));
			 g.drawImage(mem_img,100,50,250,250,this);	
			 }	
	
   //Morlet wavelet: Psi(t)=(1/sigma*(2*Pi)^1/2)*Exp(2*Pi*i*t)*Exp(-t^2/(2*sigma^2))
	public static double WaveletReal( double t) {
		double sigma = 4.0;
		return Math.cos(2.0*Math.PI*t)*
		Math.exp(-1.0*t*t/(2.0*sigma*sigma))/(sigma*Math.sqrt(2.0*Math.PI));
			 }

	public static double WaveletImag( double t )
	{	double sigma = 4.0;
		return 		Math.sin(2.0*Math.PI*t)
		*Math.exp(-1.0*t*t/(2.0*sigma*sigma))/(sigma*Math.sqrt(2.0*Math.PI));
	}
	
	public static void main(String[] args) throws IOException, FileNotFoundException {
		Frame f = new CWTapplication();
		f.setVisible(true);
	}
}