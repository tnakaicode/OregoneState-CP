c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c						
c  fourier.f: Calculates a discrete Fourier Transformation          
c       program reads input from file input.dat: y(t) separated by 
c       blanks. Output: frequency index,  real part, imaginary part

	    Program fourier
			
	      Implicit none
	      Integer max
	      Real*8 pi
	      Parameter (max = 1000,pi = 3.1415926535897932385E0)
	      Integer i, j, k
	      Real*8 input(max), real, imag
				
	      Open(9, File = 'fourier.dat', Status = 'Unknown')
c                      read from file until end-of-file or max values
	      Open(8, File = 'input.dat', Status = 'OLD')
  	    Do 10 i = 1,max
          Read(8,*,End = 20) input(i)
 10	    Continue
c                                                      frequency loop
 20	    Do 30 j = 1,i
 	        real = 0
 	        imag = 0
c                                                           sums loop
	        Do 40 k = 1,i
	          real = real + input(k) * cos( 2*pi*k*j /i )
	          imag = imag + input(k) * sin( 2*pi*k*j /i )
 40 	    Continue
 	        Write (9,*) j, real/i, imag/i
 30	    Continue
 	      Close(8)
 	      Close(9)
	      Stop 'data saved in fourier.dat'
	    End


