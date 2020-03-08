!  From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!  by RH Landau, MJ Paez, and CC BORDEIANU 
!  Copyright Princeton University Press, Princeton, 2008.
!  Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!  MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!  Support by National Science Foundation  			 
! 		
! 	 twodsol.f90:	 Solves the sine - Gordon equation for a 2D soliton	
 
Program twodsol

  Implicit none
	Double precision :: u(201, 201, 3)
	Integer :: nint
	
	Open(9, FILE = 'twodsol.dat', Status = 'UNKNOW')
	write(*, *)'		Enter an Integer from 1 to 100'
	write(*, *)' this number is proportional to time'
	write(*, *)'		time = 0 is for the Integer = 1'
	read(*, *)nint
  write(*, *)'working with input = ', nint
  call initial(u)                                        ! initialize
                                  ! output for t proportional to nint
	call solution(u, nint) 
	Stop
End Program twodsol
                                   ! initialize constants and soliton
Subroutine initial(u)
	Implicit none
    Integer :: i, j, k
	Double precision :: u(201, 201, 3), dx, dy, dt, xx, yy, dts, time
	
	Common /values/ dx, dy, dt, time, dts		
  Do i = 1, 201! clear arrays
	  Do j = 1, 201
		  Do k = 1, 3
			  u(i, j, k) = 0.0
		  End Do
	  End Do
  End Do
  dx = 14.0/200. ! initial condition
	dy = dx
	dt = dx/sqrt(2.0)
	dts = (dt/dx)**2
	yy = - 7.0
	time = 0.0
	Do	i = 1, 201
	  xx = - 7.0
		  Do	j = 1, 201
			  u(i, j, 1) = 4.0*Datan(3. - sqrt(xx*xx + yy*yy))
				xx = xx + dx
			End Do
		yy = yy + dy
	End Do
  Return 
End 
                           ! solve SGE, initial conditions in initial
Subroutine solution(u, nint)
  Implicit none	 
	Double precision :: u(201, 201, 3), dx, dy, dt, time, a2, zz,dts,a1
	Integer :: l, m, mm, k, j, i, nint
	Common/values/ dx, dy, dt, time, dts 		
	time = time + dt
              ! 2nd iteration uses d phi/dt(t=0) = 0 (G(x, y, 0) = 0)
                                ! d U/dx = 0 at - x0, x0, - y0 and y0
	Do	l = 2, 200
	  Do	m = 2, 200
		  a2 = u(m+1, l, 1) + u(m-1, l, 1) + u(m, l+1, 1) + u(m, l-1, 1)
			u(m, l, 2) = 0.5*(dts*a2 - dt*dt*DSIN(0.25*a2))
		End Do
	End Do
                                   	 	 ! the borders in 2nd iteration
	Do	mm = 2, 200
	  u(mm, 1, 2) = u(mm, 2, 2)
		u(mm, 201, 2) = u(mm, 200, 2)
		u(1, mm, 2) = u(2, mm, 2)
		u(201, mm, 2) = u(200, mm, 2)
	End Do
                                          ! the still undefined terms
  u(1, 1, 2) = u(2, 1, 2)
	u(201, 1, 2) = u(200, 1, 2)
	u(1, 201, 2) = u(2, 201, 2)
	u(201, 201, 2) = u(200, 201, 2)
       ! 3rd and following iterations use your input, loop up to nint
	Do	k = 1, nint
	  Do	l = 2, 200
		  Do	m = 2, 200
			  a1 = u(m+1, l, 2) + u(m-1, l, 2) + u(m, l+1,2) + u(m, l-1, 2)
				u(m, l, 3) = - u(m, l, 1) + dts*a1 - dt*dt*DSIN(0.25*a1)
				u(m, 1, 3) = u(m, 2, 3)
				u(m, 201, 3) = u(m, 200, 3) 
			End Do
		End Do
		Do	mm = 2, 200
		  u(mm, 1, 3) = u(mm, 2, 3)
			u(mm, 201, 3) = u(mm, 200, 3)
			u(1, mm, 3) = u(2, mm, 3)
			u(201, mm, 3) = u(200, mm, 3)
		End Do
		u(1, 1, 3) = u(2, 1, 3)
		u(201, 1, 3) = u(200, 1, 3)
		u(1, 201, 3) = u(2, 201, 3)
		u(201, 201, 3) = u(200, 201, 3)
   ! new -> old
		Do	l = 1, 201
		  Do	m = 1, 201
			  u(l, m, 1) = u(l, m, 2)
				u(l, m, 2) = u(l, m, 3)
			End Do
		End Do
                       ! Output solution at time proportional to nint
		If (k == nint) then
		  Do	i = 1, 201, 5
			  Do	j = 1, 201, 5
				  zz = DSIN(u(i, j, 3)/2.0)
					write(9, *)zz
				End Do
                  ! need blank lines to separate spatial rows for 3-D
			 write(9, *)' '
		  End Do
		Endif
		time = time + dt
  End Do
	Return
End
