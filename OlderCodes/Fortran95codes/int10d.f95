!	 int_10d.f90: Ten dimensional integration using Monte-Carlo						 
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!					
 program int10d
 implicit none										

integer ::m=16,k		! number of trials 
real *8 ::s,integ(16) 

	 s=0.0
	 
	 
	 do k=1,m
	 
	 call montecarlo(integ,k);
	 s =s+ integ(k)	 
 
	 end do

	write(*,*) s/m
	end program int10d

	subroutine montecarlo(integ, k)
	 implicit none
		integer ::i,j,k,max=65536
		real *8 ::x,y,sum,random,integ(16)
		x=0.0
		y=0.0
		sum=0.0
		
	 do i=1,max
	 
			x=0					!reset x 
	 
			do j=1,10 
			
			x= x+random() !sum of 10 x values 
			
			end do
			y=y+x*x				!square and sum up 
			
		
	 sum = sum+y/i; 
		
	 end do
	integ(k)=sum/max
	write(*,*) k,	 integ(k)
	end 

 