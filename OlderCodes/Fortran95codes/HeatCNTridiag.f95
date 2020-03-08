! HeatCNTridiag.f90:	Solution of heat equation :
! du(x,t)/dt = ct*ct *d2u(x,t)/dx2 where ct*ct=K/C*ro								 
! using	 Crank-Nicholson method		
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
Program HeatCNTridiag
				Implicit None				
				
		 Integer :: i, j,n=50,m=50
		 
Double Precision, dimension (50):: Ta,Tb,Tc,Td,a,b,c,d,x
				Double Precision :: width=1.0,height=0.1,Pi=3.1415926535,t(50,50),h,ct=1.0,r,k
				Open(9,FILE='HeatCNTridiag.dat',STATUS='UNKNOWN')
! save data in HeatCN2.dat 
		
	 
		do	i = 1,n	 
					 
			 t(i,1) = 0.0
			
		end do
		do	i = 1,m
				
			t(1,i) = 0.0
			
		end do
		!compute step sizes 
		 h	= width / ( n - 1 )
		! compute r and constants */

		k	 = height / ( m - 1 )
		r	 = ct * ct * k / ( h * h )
!write(*,*) r			
! boundary conditions 

		do j =1,m 
			 
			 t(1,j) = 0.0
			 
		 end do		
		do j=1,m
			 
				t(n,j) = 0.0
			 
		 end do
		!initial conditions
		do i = 2,n-1	 
				 
		 t(i,1) = sin( Pi * h *i)
				 
		end do		 
		! diagonal elements of the matrix A	 

		do i = 1, n	 
		Td(i) = 2.0 + 2.0 / r
		end do
		Td(1) = 1.0
		Td(n) = 1.0
		!off diagonal elements of the matrix A */
		do i = 1,n	 
		
				Ta(i) = -1.0
				Tc(i) = -1.0
		end do
			 
		Ta(n) = 0.0
		Tc(1)		= 0.0

		Tb(1) = 0.0
		Tb(n) = 0.0

		do j = 2,m
		
				do i = 2,n - 1 
				
				 Tb(i) = t(i-1,j-1) + t(i+1,j-1) + (2.0 /r	- 2.0) * t(i,j-1)
				end do
		 ! solve the dridiagonal system. 
				call Tridiag(a,b,c,d,Ta,Tb,Tc,Td,x,50) 
				do i = 1,n
					
				 t(i,j) = x(i)
					
				 end do
		end do
	 ! write data gnuplot 3D format 
		 
			 do j=1,m
				do i = 1,n; 
					write(9,*) t(i,j)		
					
				end do
			 write(9,*) ' '! empty line for gnuplot 
			end do
close(9)		
Stop'data stored in HeatCNTridiag.dat'
		end program HeatCNTridiag	 
	
 
!Subroutine Tridiag 
! solve the tridiagonal system. 
subroutine Tridiag(a, b, c, d, Ta, Tb, Tc,Td,x,n)
Implicit none
Integer :: i, j, Max=50,n
Double Precision,dimension (50) :: Ta,Tb,Tc,Td,a,b,c,d,x,h,p
 
	do i = 1, n 
	
	a(i) = Ta(i)		! vector replacement first 
	b(i) = Tb(i)
	c(i) = Tc(i)
	d(i) = Td(i)
	end do
	h(1)=c(1)/d(1)
	p(1)=b(1)/d(1)
	do	i = 2, n 
				
				h(i)=c(i)/(d(i)-a(i)*h(i-1))
				p(i)=(b(i)-a(i)*p(i-1))/(d(i)-a(i)*h(i-1))			
	end do				
			 x(n) = p(n)
	 do j = n - 1,1,-1
			x(j) = p(j) - h(j)*x(j+1)
	 end do
	
return
end
