c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation                               
c
c     rk4.f:  4th order Runge-Kutta solution for harmonic oscillator
c
 	    Program rk4Program
			
 	      Implicit none 
	      Real*8 dist, min, max, x, y(2)
	      Integer	n
c n: # eqtns, min/max in x, dist:length of x-steps, y(1): x, y(2): v

	      n = 2
        min = 0.	
        max = 10.
        dist = 0.1
        y(1) = 1.
        y(2) = 0. 
        Open(6, File = 'rk4.dat', Status = 'Unknown')
c                                          do n steps of rk algorithm
	      Do 60 x = min, max, dist
	        Call rk4(x, dist, y, n)
          Write (6, *) x, y(1)
 60	    Continue
	      Close(6)
	      Stop 'data saved in rk4.dat'
      End
c                                                      Subroutine rk4
	    Subroutine rk4(x, xstep, y, n)
	      Implicit none 
	      Real*8 deriv, h, x, xstep, y(5) 
        Real*8 k1(5), k2(5), k3(5), k4(5), t1(5), t2(5), t3(5)
     	  Integer i, n
     	  h = xstep/2.
	      Do 10 i = 1, n
	        k1(i) = xstep * deriv(x, y, i)
	        t1(i) = y(i) + 0.5*k1(i)
 10	    Continue
	      Do 20 i = 1, n
	        k2(i) = xstep * deriv(x+h, t1, i)
	        t2(i) = y(i) + 0.5*k2(i)
 20	    Continue
	      Do 30 i = 1, n
	        k3(i) = xstep * deriv(x+h, t2, i)
	        t3(i) = y(i) + k3(i)
 30	    Continue
	      Do 40 i = 1, n
	        k4(i) = xstep * deriv(x+xstep, t3, i)
	        y(i) = y(i) + (k1(i) + (2.*(k2(i) + k3(i))) + k4(i))/6.
 40	    Continue
	      Return
	    End
c                                                  Return derivatives
	    Function deriv(x, temp, i)
	      Implicit none
      	Real*8 deriv, x, temp(2)
	      Integer i
      	If (i .EQ. 1) deriv = temp(2)
	      If (i .EQ. 2) deriv = -temp(1)
	      Return
	    End
