!	 eqheat.f90: Solution of heat equation using with finite differences	 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!																																			
program heat
implicit none				
				double precision :: cons, ro, sph, thk, u(101,2)
				integer :: i, k, max
				open(9,FILE='eqheat.dat',STATUS='UNKNOWN')
! specific heat, thermal conductivity and density for iron
		sph=0.113
	thk=0.12
	ro=7.8
		cons = thk/(sph*ro)
! number of iterations
			max=30000
! At t=0 (i=1) all points are at 100 C
				do	i=1,100
					u(i,1) = 100.0
				end do
! except the endpoints which are always zero
				do	i=1,2
					u(1,i)	 = 0.0
					u(101,i) = 0.0
				end do
!
!				now start solving	 
! loop over time			
				do	k=1,max
! loop over space, endpoints stay fixed			 
					do	i=2,100
						u(i,2) = u(i,1) + cons*(u(i+1,1) + u(i-1,1)-2*u(i,1))
					end do
!
! we want to know the temperatures every 1000 time steps 
					if((MOD(k,1000)==0).or.(k==1)) then
						do	i=1,101,2
							write(9,22)u(i,2)
				 end do
						Write (9,22)
					EndIf
! recycle, new values are now old.				
					do	i=2,100
						u(i,1) = u(i,2)
					end do
				end do
 22			format (f10.6)
	close(9)
				stop 'data saved in eqheat.dat'
				end program heat
