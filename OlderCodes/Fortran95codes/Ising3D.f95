! Ising3D.f90 :Ising 3D model of magnetic dipol string									
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!
program Ising3D
implicit none
integer ::N=200,Ni=20, Nj=20, Nk=20,i,j,k,l,n1
real *8 ::sum,E, M,M2,U,U2,C,Chi,Energy,Magnetization,kT,a(20,20,20),random
Open(8, FILE='specific_heat.dat', Status='Unknown')
Open(9, FILE='susceptibility.dat', Status='Unknown')
Open(10, FILE='energy.dat', Status='Unknown')
Open(11, FILE='magnetization.dat', Status='Unknown')

! N=200 -Number of try
 !N=2001
! Ni=20, Nj=20, Nk=20 - Lattice size
! a(20,20,20) - Spin configuration
! kT - Temperature
 write(*,*) '               kT                  C(Specific Heat)'
 write(*,*) '----------------------------------------------------'
 do kT=0.5,8.0,0.10 
!Running Averages		 
! Energy
			U	 = 0.0
			U2 = 0.0
! Magnetization 
			 M	= 0.0
			 M2 = 0.0
! Set up the initial lattice configuration, 
			do	i = 1,Ni
			 do	 j = 1,Nj
				do k = 1,Nk
				a(i,j,k) = 1.0
				end do
			 end do
			end do

! Initial Energy and magnetization

			 Energy = 0.0
			 Magnetization = 0.0
			 do	 i = 1,Ni
				do j = 1,Nj
				 do k = 1,Nk
				sum=0.0 !sum of nearest neighbors
! Periodic Boundary Conditions
				if(i>1 ) then 
					sum=sum+a(i-1,j,k)
				endif
				if(i<Ni) then
					sum=sum+a(i+1,j,k)
				endif
				if(j>1 ) then
					sum=sum+a(i,j-1,k)
				 endif
				if(j<Nj) then
					sum=sum+a(i,j+1,k)
				endif
				if(k>1 ) then
					sum=sum+a(i,j,k-1)
				endif
				if(k<Nk) then
					sum=sum+a(i,j,k+1)
				endif
				Energy=Energy - sum * a(i,j,k) 
				Magnetization = Magnetization - a(i,j,k)
				 end do
				end do
			 end do
		
!	 Correct the energy
			Energy=Energy/2.0
		 do	 l = 1, N
			do n1 = 1,Ni*Nj*Nk
!Pick the node to flip at random
				 i = random()*Ni+1 
				 j = random()*Nj+1
				 k = random()*Nk+1

! Calculate	 change in energy	 when flipped
				 sum=0.0
				if(i>1 )then
					 sum=sum+a(i-1,j,k)
				endif
				if(i<Ni) then
					sum=sum+a(i+1,j,k)
				endif
				if(j>1 ) then
					sum=sum+a(i,j-1,k)
				endif
				if(j<Nj) then
					sum=sum+a(i,j+1,k)
				endif
				if(k>1 ) then
					sum=sum+a(i,j,k-1)
				endif
				if(k<Nk) then
					sum=sum+a(i,j,k+1)
				endif
			
				E =	 2.0 * sum * a(i,j,k) !energy change

! Metroplis criterium
		 
		 if(( E<0.0 ).or.( exp(-E/kT)>random() )) then 

!If accept the move upgrade spin, total magnetization, and energy
						 
						 
						 a(i,j,k)=-a(i,j,k) !flip the spin
						 Energy = Energy + E
						 Magnetization = Magnetization - 2.0 * a(i,j,k)
			endif
			end do
! Increment averages
				 M = M + Magnetization
				 M2= M2+ Magnetization * Magnetization
				 U = U + Energy 
				 U2= U2+ Energy * Energy
! Print out to files
		 if(modulo(l,N-1) == 0)then
		 
						 U= U/ l/Ni/Nj/Nk !average energy
						 U2 = U2 / l/Ni/Nj/Nk/Ni/Nj/Nk
						 C = ((U2-U*U))/kT/kT !specific heat
						 M = M / l /Ni/Nj/Nk !average magnetization
						 M2 = M2 / l /Ni/Nj/Nk/Ni/Nj/Nk
						 Chi = ((M2-M*M))/kT !susceptibility	 
	 
     write(*,*) kT, C	 
	 write(8,*)	 kT, C ! print to file the specific heat
	 write(9,*)		kT, Chi !print to file the susceptibility
	 write(10,*)	kT, U ! prin to file the average energy
	 write(11,*)	kT, abs(M)! print to file the average magnetization
		endif
end do
end do
close(8)
close(9)
close(10)
close(11)
end program Ising3D 