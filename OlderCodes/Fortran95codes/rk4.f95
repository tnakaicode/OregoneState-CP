!	 rk4.f90:	 4th order Runge-Kutta solution for harmonic oscillator			 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!			
	program oscillator
	implicit none
! declarations
! n: number of equations, min/max in x, dist:length of x-steps
! y(1): initial position, y(2):initial velocity
	real*8 ::dist, min1, max1, x, y(5)
	integer ::n
	n=2
				min1=0.0	
				max1=10.0
				dist=0.1
				y(1)=1.0
				y(2)=0.0
! open file
				open(6, File='rk4.dat', Status='Unknown')
! do n steps of Runga-Kutta algorithm
	do	x=min1, max1, dist
	call rk4(x, dist, y, n)
				write (6,*) x, y(1)
		end do
!
	close(6)
	stop 'data saved in rk4.dat'
		end program oscillator
!------------------------end of main program------------------------
!
! fourth-order Runge-Kutta subroutine 
	subroutine rk4(x, xstep, y, n)
	implicit none
! declarations
	real*8 ::deriv, h, x, xstep, y(5) 
				real*8,dimension(5) ::k1, k2,k3, k4, t1, t2, t3
			integer ::i, n
			h=xstep/2.0
	do	i = 1,n
		 k1(i) = xstep * deriv(x, y, i)
		 t1(i) = y(i) + 0.5*k1(i)
		end do
	do	i = 1,n
		 k2(i) = xstep * deriv(x+h, t1, i)
		 t2(i) = y(i) + 0.5*k2(i)
		end do
	do	i = 1,n
		 k3(i) = xstep * deriv(x+h, t2, i)
		 t3(i) = y(i) + k3(i)
		end do
	do	i = 1,n
		 k4(i) = xstep * deriv(x+xstep, t3, i)
		 y(i) = y(i) + (k1(i) + (2.*(k2(i) + k3(i))) + k4(i))/6.0
		end do
!
	return
	end
! function which returns the derivatives
	function deriv(x, temp, i)
	implicit none
!declarations
	real*8 ::deriv, x, temp(2)
	integer ::i
!
	if (i == 1) deriv=temp(2)
	if (i == 2) deriv=-temp(1)
	return
	end
