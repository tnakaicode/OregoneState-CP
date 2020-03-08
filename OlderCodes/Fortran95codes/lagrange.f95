!	 lagrange.f: Langrange interpolation of cross table									 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
				program lagrange
				implicit none
!
! Declarations
				real*8 ::inter, x, xin(9), yin(9)
				integer ::i,e
				e=9
				open(6, File='lagrange.dat', Status='Unknown')
! Input data 
				 data xin /0, 25, 50, 75, 100, 125, 150, 175, 200/
				 data yin /10.6, 16, 45, 83.5, 52.8, 19.9, 10.8, 8.25, 4.7 /		
!
! Calculate f(x)
				do	i=0, 1000
					x=i*0.2 
					write (6,*) x, inter(xin, yin, e, x)
				end do
	Close(6)
				stop 'data saved in lagrange.dat'
				end program lagrange
!
!
! Function inter
! Evaluates the interpolation function at x 
!
				function inter(xin, yin, e, x)
				implicit none
! declarations
				integer ::i, j, e
				real*8 ::inter, lambda(9), xin(9), yin(9), x
				inter = 0
				do	i=1, e
					lambda(i) = 1
					do	j=1, e
						if (i.neqv.j) then
							lambda(i) = lambda(i) * ((x - xin(j))/(xin(i) - xin(j)))
						endif
					end do
					inter = inter + (yin(i) * lambda(i))
				end do
				return
				end
