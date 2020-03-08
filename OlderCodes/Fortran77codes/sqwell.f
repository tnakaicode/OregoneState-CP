c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                             
c    			                        
c  sqwell.f: Solves the time-dependent Schroedinger equation for a 
c	           Gaussian wavepacket in a infinite square well potential
c			       Output data in gnuplot 3D format                    

      Program sqwell
			
        Implicit None
      	Real*8 psr(751, 2), psi(751, 2), p2(751)
       	Real*8 dx, k0, dt, x, pi
      	Integer i, n, max
      	Complex exc, zi
      	Common /values/dx, dt
      	Open(9, file = 'sqwell.dat', status = 'Unknown')
      	max = 750
      	pi = 3.14159265358979323846
      	zi = CMPLX(0.d0, 1.d0)
      	dx = 0.02d0
      	k0 = 17.d0*pi
      	dt = dx*dx   
c                                                  initial conditions
      	x = 0.
      	Do 10 i = 1, max+1
          exc = exp(zi*k0*x)
c                                    real, imag initial wave function
          psr(i, 1) = real(exc*exp(-0.5*(2.*(x-5.))**2.))
          psi(i, 1) = imag(exc*exp(-0.5*(2.*(x-5.))**2.))
          x = x + dx
 10   	Continue
c                                     propagate solution through time
      	Do 40 n = 1, 6000
c                                            real psi and probability
          Do 50 i = 2, max
            psr(i, 2) = psr(i, 1) - dt*(psi(i+1, 1) + psi(i-1, 1)
     1                 -2.*psi(i, 1))/(2.*dx*dx)
            p2(i) = psr(i, 1)*psr(i, 2)+psi(i, 1)*psi(i, 1)    
 50       Continue
c                                 imaginary part of the wave function
          Do 60 i = 2, max
            psi(i, 2) = psi(i, 1) + dt*(psr(i+1, 2) + psr(i-1, 2)
     1                -2.d0*psr(i, 2))/(2.d0*dx*dx)  
 60       Continue  
c                                            output  at certain times
    	  If ( Mod(n, 300) .eq. 0) Then 
          Do 80 i = 1, max+1, 15
            write(9, 11)p2(i)
 80       Continue   
          write(9, 11)   
          EndIf
c                                                          new -> old
          Do 70 i = 1, max+1
            psi(i, 1) = psi(i, 2)
            psr(i, 1) = psr(i, 2)
 70       Continue                     
 40   	Continue
 11	    Format (E12.6)
 	      Close(9)     
      	Stop 'data saved in sqwell.dat'
       End
