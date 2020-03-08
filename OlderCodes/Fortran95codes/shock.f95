! shock.f90 :Solve the Burger's shock equation using Lax-Wendroff scheme
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
program shock
implicit none
integer ::i,j,n,m
real*8 ::x,dx,dt,T_final,epsilon = 1.0,beta=0.1,u(100),u0(100)
Open(8,FILE='numerical.dat',STATUS='UNKNOWN')!	 save numerical solution data in	numerical.dat 
Open(9,FILE='initial.dat',STATUS='UNKNOWN')!save initial condition in		initial.dat 
 m = 100 !number of grid points
 !final solution
 !initial data
 !wave speed	 
 !CFL number (beta = epsilon*dt/dx)	 
			
			dx=2.0/m! space step
			dt=beta*dx/epsilon! time step
			T_final=0.15
			n=(T_final/dt)
			write(*,*) n
			!initial data
			do i=1,m
				x = i*dx
				u0(i) = 3.0*sin(3.2*x)
				write(9,*) 0.01*i, u0(i)	! save initial data
			 end do
		 !Lax-Wendroff scheme
		 do j=1,n	 
			 do i=1,m-2
u(i+1)=u0(i+1)-((u0(i+2)**2)-(u0(i)**2))*(0.25*beta)+ &
(((u0(i+2)+u0(i+1))/2.0)*((u0(i+2)**2)-(u0(i+1)**2))-((u0(i+1)+u0(i))/2.0)*((u0(i+1)**2)-(u0(i)**2)))*0.25*beta*beta
				 u(1)=0.0 
				 u(m)=0.0
				 if((modulo(j,5)==0).and.(modulo(i,4)==0))	then
					 write(8,*) u(i)
				 endif
				 u0(i) = u(i) !shift new to old
				end do
			if(modulo(j,5)==0)then
				write(8,*)' '
			endif
			end do
	 close(8)
	 close(9)
	 stop 'Numerical solution data saved in numerical.dat'
	 end program shock
