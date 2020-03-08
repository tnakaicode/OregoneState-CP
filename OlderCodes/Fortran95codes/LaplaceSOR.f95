!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation  		 
! 	
! 	 LaplaceSOR.f90:	Solve Laplace eq with finite differences c SOR

Program LaplaceSOR

  Implicit none
  Integer :: max = 40, i, j, iter
  Real*8 :: tol, aux, omega, r, p(40, 40), pi = 3.1415926535
	 
  Open(6, FILE = 'laplaceR.dat', Status = 'Unknown')      ! Data file
  omega = 1.8                                         ! SOR parameter
	                                                  ! clear the array
	Do i = 1, max									                          
	  Do j = 1, max
		  p(i, j) = 0
		End Do
	End Do
	                                                  ! p[i][0] = 100 V
  Do i = 1, max
	  p(i, 1) = + 100.0				                        
	End Do		 
  tol = 1.0																	              ! tolerance
	iter =1 
	                                                       ! iterations
  Do while ( (tol  >  0.000001).and. (iter  <= 140) )		
	  tol = 0.0
		                                                  ! x - direction
		Do i = 2, (max - 1)
		                                                  ! y - direction
		  Do j = 2, (max - 1)								                    
			  r = omega * ( p(i, j + 1) + p(i, j - 1) + p(i+ 1, j) + &
								p(i- 1, j) - 4. * p(i, j) ) / 4.0
				p(i, j) = p(i, j) + r
				If ( abs(r)  >  tol ) then
				  tol = abs(r)
				Endif
			End Do
			iter = iter + 1
	  End Do
  End Do
	                                     ! write data gnuplot 3D format
  Do i = 1, max					                     
	  Do j = 1, max 
		  write(6, *) p(i, j)
		End Do
	  write(6, *) '	'		                              
	End Do
	close(6)
	Stop 'data stored in laplaceR.dat (for gnuplot)'
End Program LaplaceSOR
