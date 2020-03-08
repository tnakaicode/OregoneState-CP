! fit.f90: Least square fit to decay spectrum														
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
			program fit
			implicit none
!
! declarations
			Integer ::i
			Real*8 ::s, sx, sy, sxx, sxy, delta, inter, slope
			Real*8 ::x(12), y(12), d(12)
!
! input value y - exponential fit y > 0			 
			Data y /32, 17, 21, 7, 8, 6, 5, 2, 2, 0.1, 4, 1/
!
! input values x
			do	i=1, 12
				 x(i)=i*10-5
			end do
!
! input value delta y - estimate
			do	i=1, 12
				 d(i)=1.0
			end do
!
! take logs of y values for exponential fit
			do i=1, 12
				 y(i)=log(y(i))
			end do
s=0.0
sx=0.0
sy=0.0
sxx=0.0
sxy=0.0
!
! calculate all the sums
			do	i=1, 12
				 s	 = s	 +				 1 / (d(i)*d(i))
				 sx	 = sx	 +			x(i) / (d(i)*d(i))
				 sy	 = sy	 +			y(i) / (d(i)*d(i))
				 sxx = sxx + x(i)*x(i) / (d(i)*d(i)) 
				 sxy = sxy + x(i)*y(i) / (d(i)*d(i))
			end do
!
! calculate the coefficients
			delta= s*sxx-sx*sx
			slope=	(s*sxy-sx*sy) / delta		
			inter=(sxx*sy-sx*sxy) / delta
			write(*,*) 'intercept=', inter
			write(*,*) 'slope=', slope
			write(*,*) 'correlation=', -sx/sqrt(sxx*s)		
			stop 'fit'
			end program fit



