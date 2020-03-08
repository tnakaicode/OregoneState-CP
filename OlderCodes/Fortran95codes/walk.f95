! walk.f90:Random walk simulation																
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
		program walk
		implicit none
! declarations
	real*8 ::random, x, y
		integer ::i, j, max
! set parameters (# of steps)
	max		= 10000
! open file
	open(6, FILE='walk.dat', Status='Unknown')
! average over 100 trials
	do	j = 1, 100
					x = 0
					y = 0
! take max steps
					do	i = 1, max
						x = x + (random()-0.5)*2.0*root2
						y = y + (random()-0.5)*2.0*root2
						Write (6,*) x, y
				  end do
 end do 
 close(6)
	Stop 'data saved in walk.dat'
	end program walk
