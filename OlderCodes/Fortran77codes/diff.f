c  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
c  by RH Landau, MJ Paez, and CC BORDEIANU 
c  Copyright Princeton University Press, Princeton, 2008.
c  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
c  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
c  Support by National Science Foundation      
c
c  diff.f:  Differentiation; forward, central & extrapolated differnc
c           results saved as x y1 y2 y3 

	    Program diff
			
	      Implicit None
c                  h stepsize for approximation, xrange and xstepsize
	      Real*8 f, h, result(3), x, xmin, xmax, xstep
				
        Open(6, File = 'diff.dat', Status = 'Unknown')
	      h = 1.e-5
	      xmin = 0.0
	      xmax = 7.0
	      xstep = 0.01
				
	      Do 10 x = xmin, xmax, xstep
	  result(1) = (f(x+h) - f(x))/h
	  result(2) = (f(x+h/2) - f(x-h/2))/h
	  result(3) = (8*(f(x+h/4)-f(x-h/4))-(f(x+h/2)-f(x-h/2)))/(3*h)
	      Write (6, 20) x, result(1), result(2), result(3)
 10 	  Continue
 20 	  Format(F5.3, TR4, F10.8, TR4, F10.8, TR4, F10.8) 
 	      Close(6)	   
 	      Stop 'data saved in diff.dat'
 	    End
c
c                                           the function to integrate
	    Function f(x)
	      Implicit none
	      Real*8 f, x
	      f = cos(x)
	      Return
	    End
	
