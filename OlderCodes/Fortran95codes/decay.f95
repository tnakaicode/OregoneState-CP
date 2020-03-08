!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation   						
! 
!   decay.f90: Spontaneous radioactive decay simulation		 
! 		
Program decay

  Implicit none                                             
  Real*8 :: r, ranDom, lambda                                 
	Integer :: i, j, h, nleft, nloop, start, seed
  
	! Set params (decay rate, initial no of atoms, seed), plant seed
	lambda = 0.01
	start = 1000 
	seed = 11168	
	h = 1
	nloop = start
	nleft = start
	open(6, File = 'decay.dat')                    ! open output 'file'
                                     ! loop over times and over atoms
	Do	j = 1, 10000                         
	  Do	i = 1, nleft
		  r = ranDom(seed)
			If (r  <= lambda) then
			  nloop = nloop -1
			Endif
		End Do        
		                                                 ! atom loop Ends
    nleft = nloop
	  Write (6, *) h, ' ', real(nleft)/start
	  h = h + 1
	  If (nleft == 0) goto 30
	End Do
  30 close(6) 
	Stop 'data saved in decay.dat'
End Program decay
