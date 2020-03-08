!	 limit.f90: determines the machine precision													 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
			program limit
			implicit none
!
! determine the machine precision
!
			integer:: I, N
			real*8 ::eps, one
! number of iterations N
			N=60
! set initial values
			eps = 1.0
			one = 1.0
! add eps to one and print result
			do	I = 1, N
				 eps = eps / 2
				 one = 1 + eps
				 write (*,*) I, one, eps
			end do	 
			stop 'limit'
			end program limit

			
