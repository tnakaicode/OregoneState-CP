
!	 sqwell.f90: Solves the time-dependent Schroedinger equation for a		 
! Gaussian wavepacket in a infinite square well potential				 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!																						 
				program sqwell
				implicit None
				real*8 ::psr(751,2),psi(751,2),p2(751)
				real*8 ::dx,k0,dt,x,pi
				integer ::i,j,n,max
				complex exc,zi
				common /values/dx,dt
				open(9,FILE='sqwell.dat',STATUS='UNKNOWN')
				max		= 750
				pi	 = 3.14159265358979323846
				zi	 = CMPLX(0.0,1.0)
				dx	 = 0.02
				k0	 = 17.0*pi
				dt	 = dx*dx	 
! clear the arrays
do i=1,751
	do j=1,2
		psr(i,j)=0.0
		psi(i,j)=0.0
		p2(i)=0.0
	end do
end do
! initial conditions
				x		= 0.0
				do	i=1,max+1
					exc			 = exp(zi*k0*x)
! real part of the initial wave function at t=0					
					psr(i,1) = real(exc*exp(-0.5*(2.0*(x-5.0))**2))
! imaginary part of the initial wave function at t=0						
					psi(i,1) = aimag(exc*exp(-0.5*(2.0*(x-5.0))**2))
					x				 = x + dx
				end do
!
! now propagate solution through time
!
				do	n=1,6000
! the real part of the wave function and the probability				
					do	i=2,max
						psr(i,2) = psr(i,1) - dt*(psi(i+1,1) + psi(i-1,1)&
											-2.0*psi(i,1))/(2.0*dx*dx)
						p2(i)		 = psr(i,1)*psr(i,2)+psi(i,1)*psi(i,1)						 
					end do
! the imaginary part of the wave function		 
					do	i=2,max
						psi(i,2) = psi(i,1) + dt*(psr(i+1,2) + psr(i-1,2)&
										 -2.0*psr(i,2))/(2.0*dx*dx)	 
					end do 
! only at certain time instants we want to know
! the probability
				if(MOD(n,300)==0) then 
						do	i=1,max+1,15
							write(9,11) p2(i)
						end do
						write(9,*) ' '	 
					endif
! new iterations are now the old ones
					do	i=1,max+1
						psi(i,1) = psi(i,2)
						psr(i,1) = psr(i,2)
					end do										 
				 end do
 11 format (E12.6)
	close(9)		 
				stop 'data saved in sqwell.dat'
				end
