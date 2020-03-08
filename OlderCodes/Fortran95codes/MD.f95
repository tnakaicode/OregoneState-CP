! MD.f90: Molecular dynamics for n atoms in 1D with	 Lennard-Jones potential,
! velocity Verlet algorithm 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!-------------------------------------------------------------------------------
! Natom - number of atoms 
! Nmax - maximum number of atoms	
! Nstep - number of time steps 
! Nprint - number of time steps between printing
! L		= box size
! h - time step 
! hover2 = h/2
! PE - potential energy	 
! KE - kinetic energy		 
! T - temperature
! fx[] - force
! x[] - positions
! vx[] - velocity

Program MD
Implicit none
Integer :: L, Natom=8, Nmax=513 , t1, t2, i, Itemp, t,	Nstep=5000,ix,Nprint=100 
Real *8	 :: h=0.004, hover2,	PE, KE,	 Temp, Tinit=10.0,x(8),fx(8,2),vx(8),w,random
!save positions, etc in MDrhl.dat
 Open(6,FILE='MDrhl.dat',STATUS='UNKNOWN') 
L = (1.*Natom)** 1/1. 
Natom =	 L** 1
!write(*,*) Natom,L
i = 0
!Set up the initial lattice configuration			 
do ix = 1,L
		i = i+1
		x(i) = ix
		!write(*,*) i,x(i)
			 
!Initial velocities according to Maxwell-Gaussian Distribution

vx(i) =(random()+random()+random()+random()+random()+random()+random()&
+random()+random()+random()+random()+random())/12.-0.5 
	vx(i) = vx(i)*sqrt(Tinit) !scale velocity with temperature
	!write(*,*) vx(i)
end do 
 !t, t+h indices
 t1 = 1
 t2 = 2
 hover2 = h/2.0
 !w = 0.0!initial virial
 PE = 0.0
 w = 0.0
! initial KE & PE via Forces
 t = 0
 KE = 0.0
 do i = 1, Natom
	KE=KE+(vx(i)*vx(i))/2
 end do
write(*,*) '          PE                           KE                         (PE+KE)'
write(*,*) '-------------------------------------------------------------------------'
write(*,*) PE , KE ,(PE+KE)

call Forces(t1, PE,x,fx)
do i = 1,Natom-1
!write(6,*) t,i,x(i)
!write(6,*) t,i,vx(i)
!write(6,*) t,i,fx(i,t1)
end do
!Main loop
		 do t = 1,Nstep	 
		 do i = 1,Natom-1 
		 ! velocity Verlet algorithm
				 call Forces(t1, PE,x,fx)
				 x(i) = x(i) + h*(vx(i) + hover2*fx(i,t1))
		 !periodic boundary conditions
			if (x(i) <= 0.) then
				x(i) = x(i) + L
			endif
			if (x(i) >= L) then
				x(i) = x(i) - L
			endif
		 end do
		 PE = 0
		 call Forces(t2, PE,x,fx)
		 KE = 0.
		 w = 0.
		 do	 i = 1,Natom-1 
						vx(i) = vx(i) + hover2*(fx(i,t1) + fx(i,t2))

!write(6,*) t,i,fx(i,t1),fx(i,t2)
!write(6,*) t,i,vx(i) 
!write(6,*) t,i,x(i)
			 KE = KE + (vx(i)*vx(i))/2
		 end do
		 Temp = 2.*KE / (3.*Natom)
!increment averages
				if (modulo(t,Nprint) == 0) then 
		 write(*,*) PE , KE ,(PE+KE)
		do i=1,Natom
		!write(6,*) t,i,x(i)
		!write(6,*) t,i,vx(i)
		end do
					
				 endif
				
				Itemp = t1 !time t and t+h
				t1 = t2
				t2 = Itemp
		end do
		close(6)
		end program md


!		Forces function
!		Compute forces, PE, and Virial; 
!		V(r) = 4*(1/r**12 - 1/r**6)

subroutine Forces( t, PE,x,fx)
implicit none
integer ::i,j,t,L,Natom=8
real *8 :: fijx, r2, invr2=0, dx, r2cut,PE,fx(8,2),x(8),x1
 L = (1.*Natom)** 1/1.	 
r2cut = 9.! Cut-off radius
! Initialize forces.
 PE = 0.
 do i=1,Natom
		fx(i,t) = 0.0
 end do
!Compute forces.
do i = 1, Natom-1
	x1=x(i)					 
	do j = i+1,Natom
	dx = x1-x(j)
!minimum image criterium
	 if(abs(dx) > 0.50*L)then
			if(dx>=0) then
			dx = dx - abs(L)
			else
			dx=dx + abs(L)
			endif	 
	 endif
	 r2 = dx**2
! Cut off
	 if(r2 < r2cut)then
		 
			 if(r2==0.) then 
				 r2=0.0001;
			 endif
			 invr2 = 1./r2	 
			 fijx =	 48.*(invr2**3-0.5)*(invr2**3)		
			 fijx = fijx*invr2*dx
			 fx(i,t) = fx(i,t) + fijx
			 fx(j,t) = fx(j,t) - fijx
			 PE = PE + 4*invr2**3*( invr2**3 - 1.)
		endif
					 end do
			end do
				
end

