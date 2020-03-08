c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation   
c    		
c  slit.f: Solves the time-dependent Schroedinger equation for  2-D 
c	        Gaussian wavepacket entering a slit (which takes some time)
c         Output saved in gnuplot 3D grid format
c
      Program slit
			
      	Implicit None
      	Real*8 psr(91, 91, 2), psi(91, 91, 2), v(91, 91), p2(91, 91)
      	Real*8 a1, a2, dt, dx, k0x, k0y, x0, y0, x, y
      	Integer i, j, max, n, time 
      	complex exc, zi  
				
c       input pos int proportional to t when want to view wave packet
      	Write(*, *)'Enter a positive integer from 1 (initial time)'
      	Write(*, *)'to 800 to get wave packet position at that time'
      	Read(*, *)time
      	Write(*, *)'processing data for time', time
      	Open(9, file = 'slit.dat', status = 'Unknown')
c                                initialize constants and wave packet
      	zi = cmplx(0.d0, 1.d0)
      	dx = 0.2d0
      	dt = 0.0025/(dx*dx)
c                                          initial momentum, position
      	k0x = 0.d0
      	k0y = 2.5d0      
      	x0 = 0.d0
      	y0 = -7.d0
      	max = 90
c                                               initial wave function
      	y = -9.d0
	      Do 90 j = 1, max+1
          x = -9.d0
          Do 10 i = 1, max+1
            exc = exp(zi*(k0x*x+k0y*y))
            a1 = exp(-0.5*(((x-x0))**2.+((y-y0))**2.))
c                           real, imag parts of initial wave function
            psr(i, j, 1) = real(a1*exc)
            psi(i, j, 1) = imag(a1*exc)
            x = x + dx
 10       Continue
          y = y + dx
 90   	Continue    
c                         set potential slit width = 50-40 = 10 units
c
	      Do 220 j = 1, max+1           
          Do 190 i = 1, max+1
          If ((j .eq. 35) .and. ((i .le. 40) .or. (i .ge. 51))) Then 
              v(i, j) = 0.5
            Else  
              v(i, j) = 0.
            EndIf         
 190      Continue 
 220   	Continue
c                                     propagate solution through time
      	Do 40 n = 1, time   
          Do 150 j = 2, max
            Do 50 i = 2, max
c                                                     Re psi and prob
              a2 = v(i, j)*psi(i, j, 1)+2.d0*dt*psi(i, j, 1)
              a1 = psi(i+1,j,1) + psi(i-1,j,1) + psi(i,j+1,1) 
							                                        + psi(i,j-1,1)
              psr(i, j, 2) = psr(i, j, 1) - dt*a1 + 2.*a2
                If ( n .eq. time) Then
                  p2(i,j)=psr(i,j,1)*psr(i,j,1)+psi(i,j,1)*psi(i,j,1)
                EndIf         
 50         Continue  
c                                        derivative = zero  at x edge
            psr(1, j, 2) = psr(2, j, 2) 
            psr(max+1, j, 2) = psr(max, j, 2)                  
 150      Continue    
c                                                            imag psi
          Do 160 j = 2, max
            Do 60 i = 2, max
              a2 = v(i, j)*psr(i, j, 2)+2.*dt*psr(i, j, 2)
              a1 = psr(i+1,j,2) + psr(i-1,j,2) + psr(i,j-1,2) 
							                                        + psr(i,j+1,2)
              psi(i, j, 2) = psi(i, j, 1)+dt*a1-2.*a2   
 60         Continue  
c                                        derivative = zero  at x edge
            psi(1, j, 2) = psi(2, j, 2) 
            psi(max+1, j, 2) = psi(max, j, 2)                  
 160      Continue         
c                                                     new -> old ones
          Do 180 j = 1, max+1  
            Do 70 i = 1, max+1
              psi(i, j, 1) = psi(i, j, 2)
              psr(i, j, 1) = psr(i, j, 2)
 70         Continue   
 180      Continue                  
 40   	Continue    
c                          output probabilities plus scaled potential
      	Do 200 j = 2, max, 3
          Do 210 i = 2, max, 2
            Write(9, 11)p2(i, j)+v(i, j)
 210      Continue  
          Write(9, 11)      
 200  	Continue
  11	  Format (E12.6)       
       	Close(9)
       	Stop 'data saved in slit.dat'
      End  
