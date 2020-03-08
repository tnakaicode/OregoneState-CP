!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation  
! 
! 	 eqstring.f90: Solution of wave equation using time stepping	
  		
Program eqstring

  Implicit none
	Real*8 :: x(101, 3)
	Integer :: i, j, k, max
	
	max = 100
	open(9, FILE = 'eqstring.dat', Status = 'Unknown')	 
	Do i = 1, 101
	  Do j = 1, 3
		  x(i, j) = 0.0
		End Do
	End Do
	                                                       ! initialize
  Do	i = 1, 80                                                 
	  x(i, 1) = 0.00125*i
	End Do
	Do	i = 81, 101
	  x(i, 1) = 0.1 - 0.005*(i- 81)
	End Do
                                                    ! first time step
	Do	i = 2, 100
	  x(i, 2) = x(i, 1) + 0.5*(x(i+1, 1) + x(i-1, 1) - 2.*x(i, 1))
	End Do
	                                                 ! other time steps
  Do k = 1, max                                          
	  Do	i = 2, 100
		  x(i,3) = 2.*x(i,2) - x(i,1) + (x(i+1,2) + x(i-1,2) - 2.*x(i,2))
	  End Do
	  Do i = 1, 101
		  x(i, 1) = x(i, 2)                                 ! new - > old
			x(i, 2) = x(i, 3)
	  End Do
    If (modulo(k, 10) == 0) then
                                         ! output data every 10 steps
      Do	i = 1, 101
		    write(9, 11) x(i, 3)
		   End Do
		   write(9, *) ' '
	  Endif
  End Do
 11	format (e12.6)
	close(9)
	Stop 'data saved in eqstring.dat (for gnuplot)'
End Program eqstring
