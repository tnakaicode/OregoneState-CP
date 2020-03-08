! twoplates.f90:	Solution of Navier-Stokes equations with finite		 
! differences for the laminar flow between two plates.					
! Uses SOR(successive over-relaxation) method in order to								
! increase the speed of convergence.																		
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
 program twoplates
 implicit none
 integer ::i,j,iter,Nxmax=400,Nymax=40
 real *8 ::V0=1.0,err,r1,r2,omega,vx(400,40),vy(400,40)
 Open(9,FILE='twopr.dat',STATUS='UNKNOWN')

 
!initial velocity
!
!grid parameters
omega=0.53;
!omega=1.78;
write(*,*)	omega
iter=0							 !number of iterations
err=1.0								 !precision
 do while ((err>0.1).and.(iter<=100))
err=0.0
do i=1,Nxmax
		do j=1,Nymax
			vx(i,j)=0.0
			vy(i,j)=0.0
		end do
end do		
!Top solid plate boundary conditions
do i=1,Nxmax
		vx(i,Nymax)=0
		vy(i,Nymax)=0
		!vx[i][Nymax]=vx[i][Nymax-1];
		!vy[i][Nymax]=vy[i][Nymax-1];
end do
!Inlet boundary conditions
do j=1,Nymax
		vx(1,j)=V0
		vy(1,j)=0
end do
!Bottom solid plate boundary conditions
do i=1,Nxmax
		vx(i,1)=0
		vy(i,1)=0
end do
!Outlet boundary conditions
do j=1,Nymax
		
		vx(Nxmax,j)=vx(Nxmax-1,j)
		vy(Nxmax,j)=vy(Nxmax-1,j)
end do
do i=2,Nxmax-1
		do j=2,Nymax-1
		vx(i+1,j)=vx(i-1,j)+vy(i,j-1)-vy(i,j+1)
		r1=omega*(vx(i+1,j)+vx(i-1,j)+vx(i,j+1)+vx(i,j-1)- &
		4*vx(i,j)- &
		0.5*vx(i,j)*(vx(i+1,j)-vx(i-1,j))- &
		0.5*vy(i,j)*(vx(i,j+1)-vx(i,j-1))+0.5*(12.0/400.0))/4.0
		vx(i,j) = vx(i,j)+r1
		r2=omega*(vy(i+1,j)+vy(i-1,j)+vy(i,j+1)+vy(i,j-1)- &
		4*vy(i,j)- &
		0.5*vx(i,j)*(vy(i+1,j)-vy(i-1,j))- &
		0.5*vy(i,j)*(vy(i,j+1)-vy(i,j-1)))/4.0
		vy(i,j) = vy(i,j)+r2
!Top solid plate
if((i>1).and.(i<=Nxmax).and.(j==Nymax))then
	
		vx(i,j)=0
		vy(i,j)=0
!vx(i,j)=vx(i,j-1)
!vy(i,j)=vy(i,j-1)
endif
!Bottom solid plate
if((i>1).and.(i<=Nxmax).and.(j==1))then
	
		vx(i,j)=0
		vy(i,j)=0
endif
! Outlet
if((i==Nxmax).and.(j>1).and.(j<=Nymax))then
		
		vx(i,j)=vx(i-1,j)
		vy(i,j)=vy(i-1,j)
endif
! Inlet
if((i==1).and.(j>1).and.(j<=Nymax))then
	
		vx(i,j)=V0
		vy(i,j)=0

endif
err=max(err,abs(r1))
end do
end do
iter=iter+1
end do

do i=1,Nxmax
		do j=1,Nymax
write(9,*) iter, j, vx(i,j), 6.0*(j/40.0)*(1.0-(j/40.0))
end do
write(9,*) ' '
end do
close(9)
stop'data saved in twopr.dat file '
end program twoplates
