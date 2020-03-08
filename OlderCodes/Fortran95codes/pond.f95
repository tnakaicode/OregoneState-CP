!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation  			 
! 
! 	 pond.f90:  pi via Monte-Carlo integration (throwing stones)

Program pond

	Implicit none  
	Real*8 :: area, x, y, ranDom
	Integer :: i, max, pi
	
	max = 2000
                       ! open file, set initial value, seed generator
	Open(6, File = 'pond.dat', Status = 'Unknown')
	pi = 0
	                                                          ! execute
	Do i = 1, max       
	  x = ranDom()*2 - 1
		y = ranDom()*2 - 1 
		If ((x*x + y*y)  <= 1) then
		  pi = pi + 1
		Endif
		area = 4. * pi/Real(i)
		write(6, *) i, area
	End Do
	close(6)
	Stop 'data saved in pond.dat'
End Program pond
