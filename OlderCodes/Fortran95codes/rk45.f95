!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation  		 
! 		
!   rk45.f90: ODE solver via variable step size rk, Tol = error
  
Program Rk45 

  Implicit none
  Real *8 :: h, t, s, s1, hmin, hmax, Tol = 2*1E - 7, Tmin = 0., &
	           Tmax = 10.
  Real *8, dimension(2) :: w, y, FReturn, ydumb, k1, k2, k3, k4, &
	                         k5, k6, err
	Integer :: i, Ntimes = 10  
	
  Open(6, FILE = 'rk45.dat', Status = 'Unknown')
	                                                       ! initialize
  y(1) = 3.0 ; y(2) = - 5.0				                              
  h = (Tmax - Tmin) / Ntimes				      ! tentative number of steps
  hmin = h/64
  hmax = h*64										      ! minimum and maximum step size
  t = Tmin
	                                                   ! output to file
       
   Do while (t < Tmax)
	    
		write(6, *) t, y(1)	                                    
		If ( (t + h)  >  Tmax ) then 
			h = Tmax - t	! the last step
		EndIf
		                             ! evaluate both RHSs and Return in F
		call f(t, y, FReturn)		            
		Do i = 1, 2 
		  k1(i) = h*FReturn(i)		
		  ydumb(i) = y(i) + k1(i)/4
		End Do
		call f(t + h/4, ydumb, FReturn)
		Do i = 1, 2 
		  k2(i) = h*FReturn(i)
		  ydumb(i) = y(i) + 3*k1(i)/32 + 9*k2(i)/32
		End Do
		call f(t + 3*h/8, ydumb, FReturn)
		Do i = 1, 2 
		  k3(i) = h*FReturn(i)	
		  ydumb(i) = y(i) + 1932*k1(i)/2197 - 7200*k2(i)/2197. &
			                                              + 7296*k3(i)/2197
		End Do
		call f(t + 12*h/13, ydumb, FReturn)
		Do i = 1, 2
		  k4(i) = h*FReturn(i) 
		  ydumb(i) = y(i) + 439*k1(i)/216-8*k2(i) &
			                                + 3680*k3(i)/513-845*k4(i)/4104
		End Do
		call f(t + h, ydumb, FReturn)
		Do i = 1, 2 
		 k5(i) = h*FReturn(i)
		 ydumb(i) = y(i) - 8*k1(i)/27 + 2*k2(i) - 3544*k3(i)/2565 &
                                      + 1859*k4(i)/4104 - 11*k5(i)/40
		End Do							
		call f(t + h/2, ydumb, FReturn)
		  Do i = 1, 2 
		    k6(i) = h*FReturn(i) 
			  err(i) = abs( k1(i)/360 - 128*k3(i)/4275 - 2197*k4(i)/75240 &
                                          + k5(i)/50. + 2*k6(i)/55 )
		 End Do
		 If ((err(1) < Tol).or.(err(2) < Tol).or.(h <= 2*hmin))	then 
		                                           ! accept approximation
       Do i = 1, 2                                    
				  y(i) = y(i) + 25*k1(i)/216. + 1408*k3(i)/2565. &
                                       + 2197*k4(i)/4104. - k5(i)/5.
			End Do
			t = t + h
		Endif
		If (( err(1) == 0).or.(err(2) == 0)) then
			s = 0! trap division by 0
		else 
			s = 0.84*Tol*h/err(1)**0.25                  ! step size scalar
		Endif
		If ( (s  <  0.75).and. (h  >  2*hmin) )then
		  h = 	h/2.! reduce step
		else If ( (s  >  1.5).and. (2* h  <  hmax) )then
		  h = h*2.	! increase step
		Endif
		                                                       ! End loop
  End Do                                                          
  close(6)
  Stop'Data stored in rk45.dat'
End Program Rk45
                                           ! PLACE YOUR FUNCTION HERE
subroutine f(t, 	 y, FReturn) 
  Implicit none; Real *8 t, y(2), FReturn(2)
	FReturn(1) = y(2)! RHS of first equation
	FReturn(2) = - 100*y(1) - 2*y(2) + 10*sin(3*t)! RHS of 2nd equation
  Return
End
