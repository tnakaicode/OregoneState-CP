!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation  		 
! 		 
! soliton.f90: Solves the KdeV Equation via finite differences
!
Program soliton

  Implicit None
	Real*8 :: ds, dt, max, mu, eps, u(131, 3)
	parameter(ds = 0.4, dt = 0.1, max = 2000, mu = 0.1, eps = 0.2) 
 ! delta t, delta x, time steps, mu and eps from KdeV equation
	Real*8 :: a1, a2, a3, fac, time
	Integer :: i, j, k
	
	open (9, FILE = 'soliton.dat', Status = 'Unknown')
	                                                ! Initial condition
  Do	i = 1, 131                                         
	  u(i, 1) = 0.5*(1. - tanh(0.2*ds*(i- 1) - 5.))
	End Do
	                                                        ! Endpoints
  u(1, 2)	 = 1.                                                 
	u(1, 3)	 = 1.
	u(131, 2) = 0.
	u(131, 3) = 0.
	fac	 = mu*dt/(ds**3.)
	time = dt
	                                                       ! first step
  Do	i = 2, 130                                                
		a1 = eps*dt*(u(i+ 1, 1) + u(i, 1) + u(i- 1, 1))/(ds*6.d0)
		If ((i > 2).and.(i <= 129)) then
			 a2 = u(i+ 2, 1) + 2.*u(i- 1, 1) - 2.*u(i+ 1, 1) - u(i- 2, 1)
		Endif
		If ((i == 2).or.(i == 130)) then 
		  a2 = u(i- 1, 1) - u(i+ 1, 1)
		Endif
		a3 = u(i+ 1, 1) - u(i- 1, 1)
		u(i, 2)	 = u(i, 1) - a1*a3 - fac*a2/3.d0
	End Do
	                                                  ! all other times
  Do	j = 1, max                                           
	  Do	i = 2, 130
		  a1 = eps*dt*(u(i+ 1, 2) + u(i, 2) + u(i- 1, 2))/(3.d0*ds)
			If ((i > 2).and.(i <= 129)) then
			  a2 = u(i+2,2) + 2.d0*u(i-1,2) - 2.d0*u(i+1,2) - u(i-2,2)
			Endif
			If ((i == 2).or.(i == 130)) then 
			  a2 = u(i- 1, 2) - u(i+ 1, 2)
			Endif 
			a3 = u(i+ 1, 2) - u(i- 1, 2)
			u(i, 3) = u(i, 1) - a1*a3 - 2.d0*fac*a2/3.d0
			u(1, 3) = 1.d0
		End Do
		                                                     ! new -> old
    Do	k = 1, 131                                      
			u(k, 1) = u(k, 2)
			u(k, 2) = u(k, 3)
		End Do
		                                    ! output every 200 time steps
    If (modulo(j, 200) == 0) then  
			Do	k = 1, 131
			  write(9, 22)u(k, 3)
			End Do	
	  	write(9, 22)
		EndIf
		time = time + dt
  End Do
 22		format(f10.6)
	close(9)				 
	Stop 'data saved in soliton.dat (for gnuplot)'
End Program soliton
