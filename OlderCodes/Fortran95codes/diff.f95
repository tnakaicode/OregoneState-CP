! diff.f90:	 Differentiation	using forward, central and								
!		 extrapolated difference methods														
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
	program diff
	implicit none
! Declarations
! h stepsize for approximation, xrange and xstepsize
!
	Real*8 ::f, h, result(3), x, xmin, xmax, xstep
				open(6, File='diff.dat', Status='Unknown')
	h			= 1.e-5
	xmin	= 0.0
	xmax	= 7.0
	xstep = 0.01
	do	x=xmin, xmax, xstep
		 result(1) = (f(x+h) - f(x))/h
		 result(2) = (f(x+h/2) - f(x-h/2))/h
		 result(3) = (8*(f(x+h/4)-f(x-h/4)) - (f(x+h/2)-f(x-h/2)))/(3*h)
		 write (6, 20) x, result(1), result(2), result(3)
		end do
20	Format(F5.3, TR4, F10.8, TR4, F10.8, TR4, F10.8) 
	close(6)		 
	stop 'data saved in diff.dat'
	end program diff
!
! the function we want to integrate
	function f(x)
		 implicit none
		 Real*8 f, x
		 f = cos(x)
		 return
	end
	
