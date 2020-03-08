/* From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
   by RH Landau, MJ Paez, and CC BORDEIANU 
   Copyright Princeton University Press, Princeton, 2008.
   Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
   MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
   Support by National Science Foundation                              
   */
 // CompressDaub.java
import java.io.*;
import java.util.*;
import java.awt.image.*;
import java.awt.*;
import java.awt.event.*;

public class CompressDaub extends Frame { 
	Image orig_img,mem_img;	
	double a[]=new double[512*512+1];
	int b[]=new int[512*512+1];
	int bb[]=new int[512*512+1];
	int nn[]=new int[3];
	int n=512;
	double tol;// threshold
	
	public CompressDaub()  {
		setSize(640,480);
		addWindowListener(new WindowAdapter()
		{ public void windowClosing(WindowEvent event) { System.exit(0); }});
	}
	
	public void paint(Graphics g) {
		int i, k;
		double max;
		g.setColor(Color.blue);
		g.drawString("Original Image",80,60);
		g.drawString("Compressed Image using Daubechies4 wavelet with 30% treshold",245,60);	
		g.drawString("The treshold percent can be modified in the code",170,430); 
		// read the data of original image from a file 
		//called ph.dat and save in array a[].
		try {
			FileInputStream fis = new FileInputStream("ph.dat");
			BufferedReader br = new BufferedReader(new InputStreamReader(fis));					
			StreamTokenizer st = new StreamTokenizer(br);
			i=1;
			while (st.nextToken()!= StreamTokenizer.TT_EOF) {
				a[i]=st.nval;
				i++;
			}
		}
		catch(IOException e) {
		System.out.println(" File error:"+e+" ");	
		}
		for(i=1;i<512*512;i++){
			b[i]=(int)a[i]<<24;
		}
		// create original image from  array of integers b[]
		orig_img=createImage(new MemoryImageSource(512,512,b,0,512));
		g.drawImage(orig_img,10,80,250,250,this);                  // draw original image
		max=a[1];
		for(i=1;i<512*512;i++)  {
			if(a[i]>max)
				max=a[i];
		}
		tol=30*(max/100.0);                            //here you can change the treshold
		nn[1]=512;
		nn[2]=512;
		wtn(a,nn,2,1,tol);// apply direct DWT
		wtn(a,nn,2,-1,tol);//apply inverse DWT
		for(i=1;i<512*512;i++)  bb[i]=(int)a[i]<<24;
		// create compressed image from  array of integers bb[]
		mem_img=createImage(new MemoryImageSource(512,512,bb,0,512));
		g.drawImage(mem_img,300,80,250,250,this);                // draw compressed image
	}	
	
	void wtn(double x[], int nn[], int ndim, int isign,double tol) {
	/* ndim-dimensional discrete wavelet transform. This routine implements the pyramid 
	algorithm,replacing x[1..n] by its wavelet transform (for isign=1),or performing 
	the inverse operation (for isign=-1). Note that n must be an integer power of 2
	Adapted from Numerical Recipes. Replaces a by its ndim-dimensional discrete wavelet 
	transform, if isign is input as 1. Here 	nn[1..ndim] is an integer array 
	containing the lengths of each dimension (number of real values), which MUST all be 
	powers of 2. a is a real array of length equal to the product of these lengths, in 
	which the data are stored as in a multidimensional real array. If isign is input
	as -1, a is replaced by its inverse wavelet transform.*/
		int i1,i2,i3,k,n,nnew,nprev=1,nt,ntot=1;
		int idim;
		double temp[]=new double [512*512+1];
		for (idim=1;idim<=ndim;idim++) ntot *= nn[idim];
		for (idim=1;idim<=ndim;idim++) {                  //Main loop over the dimensions.
			n=nn[idim];
			nnew=n*nprev;
			if (n > 4) {
				for (i2=0;i2<ntot;i2+=nnew) {
					for (i1=1;i1<=nprev;i1++) {
						for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) temp[k]=x[i3];
						                //Copy the relevant row or column or etc. into workspace.
						if (isign >= 0) {                  //Do one-dimensional wavelet transform.
							for(nt=n;nt>=4;nt >>= 1) daub4(temp,nt,isign,tol);
						} 
						else {                                            //Or inverse transform.
							for(nt=4;nt<=n;nt <<= 1) daub4(temp,nt,isign,tol);
						}
						for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) x[i3]=temp[k];
						} } }
			nprev=nnew;
			}
			}
			
			public static void daub4(double x[], int n, int isign, double tol) {
			//Applies the Daubechies 4-coefficient wavelet matrix to 
			//data vector x[1..n] (for isign=1) or applies its transpose 
			//(for isign=-1). Used hierarchically by routines wtn.
			double C0 =0.4829629131445341;                      //Daubechies 4 coefficients
			double C1 =0.8365163037378079;
			double C2 =0.2241438680420134;
			double C3 =-0.1294095225512604;
			double temp[]=new double [n+1];
			int nh,nh1,i,j;
			if (n < 4) return;
			//quick division by 2 using bit operator >> , nh = n>>1 = n/2
			nh1=(nh=n >> 1)+1;
			if (isign >= 0) { //Apply .lter.
				for (i=1,j=1;j<=n-3;j+=2,i++) {
					temp[i]= C0*x[j]+C1*x[j+1]+C2*x[j+2]+C3*x[j+3];
					temp[i+nh] = C3*x[j]-C2*x[j+1]+C1*x[j+2]-C0*x[j+3];
					if (Math.abs(temp[i+nh]) < tol) temp[i+nh] = 0.0;               //threshold
					}
					temp[i] = C0*x[n-1]+C1*x[n]+C2*x[1]+C3*x[2];
					temp[i+nh] = C3*x[n-1]-C2*x[n]+C1*x[1]-C0*x[2];
					if (Math.abs(temp[i+nh]) < tol) temp[i+nh] = 0.0;               //threshold
					} 
					else { //Apply transpose 
						temp[1] = C2*x[nh]+C1*x[n]+C0*x[1]+C3*x[nh1];
						temp[2] = C3*x[nh]-C0*x[n]+C1*x[1]-C2*x[nh1];
						for (i=1,j=3;i<nh;i++) {
							temp[j++] = C2*x[i]+C1*x[i+nh]+C0*x[i+1]+C3*x[i+nh1];
							temp[j++] = C3*x[i]-C0*x[i+nh]+C1*x[i+1]-C2*x[i+nh1];
							}
					}
					for (i=1;i<=n;i++)  x[i]=temp[i];
			}
			
	public static void main(String[] args) throws IOException,FileNotFoundException {
		Frame f = new CompressDaub();
		f.setVisible(true);
	}
	}