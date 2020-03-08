c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                              
c							       
c  harmos.f: t-dependent Schroed eqtn for Gaussian in harmonic oscillator V

      Program harmos
			
      	Implicit None
        Real*8 psr(751,2), psi(751,2), v(751), p2(751), pi,dx,k0,dt,x
      	Complex exc, zi
      	Integer max, i, n
				
      	Open(9, file = 'harmos.dat', status = 'Unknown') 
      	pi = 3.1415926535897932385E0
      	zi = cmplx(0., 1.)
      	dx = 0.02
c                                   k0 = initial wave packet momentum
      	k0 = 3 * pi
      	dt = dx*dx/4.
      	max = 750
c                                                  initial conditions
      	x = -7.5
      	Do 10 i = 1, max+1
          exc = exp(zi*k0*x)
c                           real, imag parts of initial wave function
          psr(i, 1) = real(exc*exp(-0.5*(x/0.5)**2.))
          psi(i, 1) = imag(exc*exp(-0.5*(x/0.5)**2.))
          v(i) = 5.*x*x 
          x = x + dx
 10   	Continue
c                                     propagate solution through time
        Do 40 n = 1, 20000
c                                     real psi, probability, imag psi
          Do 50 i = 2, max
            psr(i, 2) = psr(i, 1)-dt*(psi(i+1, 1)+psi(i-1, 1)
     1                -2.d0*psi(i, 1))/(dx*dx)+dt*v(i)*psi(i, 1)
            p2(i) = psr(i, 1)*psr(i, 2)+psi(i, 1)*psi(i, 1)              
 50       Continue  
          Do 60 i = 2, max
            psi(i, 2) = psi(i, 1)+dt*(psr(i+1, 2)+psr(i-1, 2)
     1                -2.d0*psr(i, 2))/(dx*dx)-dt*v(i)*psr(i, 2) 
 60       Continue 
c                                        output every 2000 time steps
          If ( (n .eq. 1) .or. (Mod(n, 2000) .eq. 0)) Then
            Do 80 i = 1, max+1, 10
              Write(9, 11)p2(i) + 0.0015*v(i)
 80         Continue  
            Write(9, 11)   
          EndIf
c                                                          new -> old
          Do 70 i = 1, max+1
            psi(i, 1) = psi(i, 2)
            psr(i, 1) = psr(i, 2)
 70       Continue                     
 40     Continue     
 11	    Format(E12.6)
 	      Close(9)
      	Stop 'data saved in harmos.dat for gnuplot'
      End
                       
