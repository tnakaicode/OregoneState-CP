!	 slit.f90: Solves the time-dependent Schroedinger equation for a	
!	 two-dimensional Gaussian wavepacket entering a slit								 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!			
				program slit
				implicit none
				real*8 ::psr(91,91,2),psi(91,91,2),v(91,91),p2(91,91)
				real*8 ::a1,a2,dt,dx,k0x,k0y,x0,y0,x,y
				integer i,j,k,max,n,time 
				complex exc,zi		
! input a positive integer which is proportional to the time
! you want to see the position of the wave packet.			 
				write(*,*)'Enter a positive integer from 1(initial time)'
				write(*,*)'to 800 to get wave packet position at that time'
				read(*,*)time
				write(*,*)'processing data for time',time
				open(9,FILE='slit.dat',STATUS='UNKNOWN')
! initializes the constant values and the wave packet			 
				zi	 = cmplx(0.0,1.0)
				dx	 = 0.2
				dt	= 0.0025/(dx*dx)
! initial momentum, position				
				k0x	 = 0.0
				k0y	 = 2.5			
				x0	 = 0.0
				y0	 = -7.0
				max	 = 90
! clear the arrays
do i=1,91
	do j=1,91
		do k=1,2
			psi(i,j,k)=0.0
			psr(i,j,k)=0.0
		end do
	end do
end do
! initial wave function
				y		= -9.0
				
	do	j=1,max+1
					x=-9.0D0
					do	i=1,max+1
						exc				 = exp(zi*(k0x*x+k0y*y))
						a1 = exp(-0.5*(((x-x0))**2+((y-y0))**2))
! real part of the initial wave function
						psr(i,j,1) = real(a1*exc)
! imaginay part of the initial wave function
						psi(i,j,1) = aimag(a1*exc)
						x					 = x + dx
					end do
					y = y + dx
		end do		
!
! set up the potential slit width: 50-40=10 units			 
!
	do	j=1,max+1						
					Do	i=1,max+1
						if((j==35).and.((i<40).or.(i>51)))then 
							v(i,j) = 0.5
						else	
							v(i,j) = 0.0
						endif					
					end do 
			end do
!
! propagate solution through time			
!
do	n=1,time	 
! compute real part of wave packet and probability							
					do	j=2,max
						do	i=2,max
							a2 = v(i,j)*psi(i,j,1)+2.0D0*dt*psi(i,j,1)
							a1 = psi(i+1,j,1)+psi(i-1,j,1)+psi(i,j+1,1)+psi(i,j-1,1)
								psr(i,j,2) = psr(i,j,1)-dt*a1+2.0*a2
								if(n==time) then
									p2(i,j) = psr(i,j,1)*psr(i,j,1)+psi(i,j,1)*psi(i,j,1)	 
								endif					
						end do	
! at x edges derivative is zero	 
						psr(1,j,2)		 = psr(2,j,2) 
						psr(max+1,j,2) = psr(max,j,2)									 
					end do	 
! imaginary part of wave packet is next									 
					do	j=2,max
						do	i=2,max
							a2 = v(i,j)*psr(i,j,2)+2.0*dt*psr(i,j,2)
							a1 = psr(i+1,j,2)+psr(i-1,j,2)+psr(i,j-1,2)+psr(i,j+1,2)
							psi(i,j,2) = psi(i,j,1)+dt*a1-2.0*a2	 
						end do
! at x edges derivative is zero	 
						psi(1,j,2)		= psi(2,j,2) 
						psi(max+1,j,2) = psi(max,j,2)									 
				 end do					
! new iterations are now the old ones, recycle
					do	j=1,max+1	 
						do	i=1,max+1
							psi(i,j,1) = psi(i,j,2)
							psr(i,j,1) = psr(i,j,2)
						end do	 
					end do									
 end do		 
!
! write the probabilities plus potential multiplied by 0.025
! in disk file, to give a volume sensation of the potential 
!
				do	j=2,max,3
					do	i=2,max,2
						write(9,11)p2(i,j)+v(i,j)
					end do	
					write(9,*) ' '			
				end do
 11 format (E12.6)			 
				close(9)
				stop 'data saved in slit.dat'
				end	 
