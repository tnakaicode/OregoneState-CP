! DAUB4.f90 :1D Daubechies 4-coefficient wavelet transform
! Subroutines wt1 and daub4 adapted from "Numerical Recipes in Fortran 77"
!
!	 From: "A SURVEY OF COMPUTATIONAL PHYSICS" 
!		by RH Landau, MJ Paez, and CC BORDEIANU 
!		Copyright Princeton University Press, Princeton, 2008.
!		Electronic Materials copyright: R Landau, Oregon State Univ, 2008;
!		MJ Paez, Univ Antioquia, 2008; and CC BORDEIANU, Univ Bucharest, 2008.
!		Supported by the US National Science Foundation															 
!		
!Uses wrap around not to lose information

program DAUB4a
implicit none
integer :: i, n = 1024
Real::x(1024)!data vector
Open(6, File='DAUB4.dat', Status='Unknown')
do i = 1,n				!	 DAUB4 wavelet 10 + 58
			 x(i) = 0.0
			 x(10) = 1.0
			 x(58) = 1.0
end do
call wt1(x,n,-1)		!	 inverse DWT
do i = 1,n
	 write(6,*) i,x(i)
end do
close(6)
stop'data stored in DAUB4.dat file'
end program daub4a
subroutine wt1(a,n,isign)
integer ::isign,n,nn
real	:: a(n)
!One-dimensional discrete wavelet transform. This routine implements the pyramid algorithm,
!replacing a(1:n) by its wavelet transform (for isign=1), or performing the inverse
!operation (for isign=-1). Note that n MUST be an integer power of 2. The subroutine
!daub4, whose actual name must be supplied in calling this routine, is the underlying
!wavelet filter. 

if (n<4) return
if (isign>0) then !Wavelet transform.
nn=n !Start at largest hierarchy,
1 if (nn>4) then
call daub4(a,nn,isign) !and work towards smallest.
nn=nn/2
goto 1
endif
else !Inverse wavelet transform.
nn=4 !Start at smallest hierarchy,
2 if (nn<n) then
call daub4(a,nn,isign)
nn=nn*2 !and work towards largest.
goto 2
endif
endif
return
end 
subroutine	daub4(a,n,isign)
integer n,isign,Nmax,nh,nh1,i,j !Nmax is the maximum allowed value of n.
parameter (C0=0.4829629131445341,C1=0.8365163037378079,&
 C2=0.2241438680420134,C3=-0.1294095225512604,Nmax=1024)

real	:: a(n),C3,C2,C1,C0,wksp(Nmax)

!Applies the Daubechies 4-coefficient wavelet filter to data vector a(1:n) (for isign=1) or
!applies its transpose (for isign=-1). Used hierarchically by routines wt1 and wtn.

if(n<4)return

nh=n/2
nh1=nh+1
if (isign>=0) then !Apply filter.
i=1
do	j=1,n-3,2
wksp(i)=C0*a(j)+C1*a(j+1)+C2*a(j+2)+C3*a(j+3)
wksp(i+nh)=C3*a(j)-C2*a(j+1)+C1*a(j+2)-C0*a(j+3)
i=i+1
enddo 
wksp(i)=C0*a(n-1)+C1*a(n)+C2*a(1)+C3*a(2)
wksp(i+nh)=C3*a(n-1)-C2*a(n)+C1*a(1)-C0*a(2)
else !Apply transpose filter.
wksp(1)=C2*a(nh)+C1*a(n)+C0*a(1)+C3*a(nh1)
wksp(2)=C3*a(nh)-C0*a(n)+C1*a(1)-C2*a(nh1)
j=3
do	i=1,nh-1
wksp(j)=C2*a(i)+C1*a(i+nh)+C0*a(i+1)+C3*a(i+nh1)
wksp(j+1)=C3*a(i)-C0*a(i+nh)+C1*a(i+1)-C2*a(i+nh1)
j=j+2
end do 
endif
do	i=1,n
a(i)=wksp(i)
end do 
return
end 