!	 overflow.f90: determine overflow and underflow limits											
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
			program overflow
			implicit none
!
! determine where overflow and underflow occur
!
			integer ::I, N
			real*8 ::under, over
! number of iterations N, might not be big enough
			N=1024
! set initial values
			under = 1.0
			over	= 1.0
! calculate underflow and overflow, print output to screen
			do I = 1, N
				 under = under / 2
				 over = over * 2
				 write (*,*) I, over, under
			end do	 
			stop 'overflow'
			end program overflow

			
