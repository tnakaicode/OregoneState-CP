! harmos.f90: Solves the time-dependent Schroedinger equation for a	 
! Gaussian wavepacket in a quadratic potential											
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
				Program harmos
				Implicit None
				Real*8 :: psr(750,2), psi(750,2), v(750), p2(750)
				Real*8 :: pi,dx,k0,dt,x 
				Complex :: exc,zi
				Integer :: max,i,j,n
				Open(9,FILE='harmos.dat',STATUS='UNKNOWN') 
				pi	 = 3.1415926535897932385E0
				zi	 = cmplx(0.0,1.0)
				dx	 = 0.02
! k0 is the initial momentum given to the wave packet			 
				k0	 = 3 * pi
				dt	 = dx*dx/4.0
				max		= 750
do i=1,max
	do j=1,2
		psi(i,j)=0.0
		psr(i,j)=0.0
	end do
end do

! initial conditions
				x		= -7.5
				Do	i=1,max
					exc = exp(zi*k0*x)


! real part of initial wave function
					psr(i,1) = real(exc*exp(-0.5*(x/0.5)**2))
! imaginary part of initial wave function
					psi(i,1) = aimag(exc*exp(-0.5*(x/0.5)**2))
! the potential
					v(i)		 = 5.0*x*x 
					x				 = x + dx
				end do
!
! now propagate solution through time
!			 
				Do	n=1,20000
! the real part of the wave packet is computed here					
! and the probability P2
					Do	i=2,max-1
						psr(i,2) = psr(i,1)-dt*(psi(i+1,1)+psi(i-1,1)-2.0*psi(i,1))/(dx*dx)+dt*v(i)*psi(i,1)
						 p2(i)	= psr(i,1)*psr(i,2)+psi(i,1)*psi(i,1)							 
					end do
! same thing for the imaginary part of the wave packet										 
					Do	i=2,max-1
					 psi(i,2) = psi(i,1)+dt*(psr(i+1,2)+psr(i-1,2)-2.0*psr(i,2))/(dx*dx)-dt*v(i)*psr(i,2) 
					end do
! every 2000 time steps we want to see the probability density				
					If((n==1).or.(modulo(n,2000)==0)) Then
						Do	i=2,max-1,10
							Write(9,11) p2(i) + 0.0015*v(i)
						end do	
						Write(9,*) ' '	
					EndIf
! new iterations are now the old ones
					Do	i=1,max
						psi(i,1) = psi(i,2)
						psr(i,1) = psr(i,2)
					end do										 
 end do			
 11 Format(E12.6)
	Close(9)
				Stop 'data saved in harmos.dat'
				End
											 
